library(tidyverse)
library(ggpubr)
library(ordinal)
library(optimx)
library(stats)
library(data.table)

rm(list = ls())

# functions
# Different classes of weighting functions ============================
kumaraswamy_weights <- function(steps, a, b){
  kum_weights <- rep(0, length(steps) - 1)
  for (i in 2:length(steps)){
    kum_weights[i-1] = (1 - steps[i-1]^a)^b - (1 - steps[i]^a)^b
  }
  return(kum_weights)
}

kum_par <- c(1,1, 0.6,1, 0.9,1, 1,0.5, 5,0.9, 0.5,0.5, 0.9,0.9, 5,5, 3,6, 2,6)
dim(kum_par) <- c(2, 10)
kum_par <- t(kum_par)
colnames(kum_par) <- c("a", "b")
kum_par <- kum_par[1, ]

steps <- c(0, 1/6, 2/6, 3/6, 4/6, 5/6, 6/6)

f_kum <- function(x, steps, a, b) {
  norm_vec <- t(replicate(nrow(x), kumaraswamy_weights(steps, a, b)))
  norm_vec[is.na(x)] <- 0
  rowSums(x * norm_vec, na.rm = TRUE) / rowSums(norm_vec)
}

objective_fun <- function(params, df, steps) {
  a <- params[1]
  b <- params[2]
  
  df$tmp <- f_kum(as.matrix(df[,c("mo", "tu", "we", "th", "fr", "sa")]), 
                  steps, a, b)
  
  model <- clm(os ~ tmp, data = df, family = "binomial")  
  
  return(AIC(model))  
}

N <- 10000

# specific data set ========= 1 ===========

#Here choose the csv, it can be with real p1204 or desired only
all_data <- read_csv("phase4_current_results_p1204.csv", col_types = "fnnnnnnnnnnnncc")
#Here choose for which data you want the model: 
#n<8 for all data, n==7 for only full weeks and n<7 for only unfull weeks
data_by_weeks_n <- all_data  %>% filter(n < 8)
#Here choose what content you want: 
#week < 26 for all content, week < 14 for nature only and week > 13 for slowmo only
data_by_weeks <- data_by_weeks_n %>% filter(week < 26)
#if you want to exclude some testers you can do this after this line
file_name = "bootstrap_n_less_8_week_less_26.rds"
data_by_weeks$os <- factor(data_by_weeks$q2, ordered = TRUE, 
                           levels = c(1, 2, 3, 4, 5))

best_AIC <- 10^9
opt_param <- 0
  init_params <- kum_par
  lower_bounds <- c(0.00001, 0.00001)
  opt_data <- data_by_weeks 
  
  opt_result <- optimx(init_params, objective_fun, method="L-BFGS-B", 
                       lower=lower_bounds, df = opt_data, steps = steps)
  
  best_params <- opt_result
  print(best_params)
  if (best_params$value < best_AIC)
  {
    best_AIC <- best_params$value
    opt_param <- best_params
  }

data_by_weeks$f_best_fun <- f_kum(as.matrix(
  data_by_weeks[,c("mo", "tu", "we", "th", "fr", "sa")]), 
  steps, opt_param$a, opt_param$b)
opt_model <- clm(os ~ f_best_fun, data = data_by_weeks, family = "binomial")
summary(opt_model)

ggplot(NULL, aes(steps[2:7], kumaraswamy_weights(steps, opt_param$a, opt_param$b))) + 
  geom_point()

# 1. Predict probabilities
new_data <- data.frame(f_best_fun = data_by_weeks$f_best_fun)
predicted_probs <- predict(opt_model, newdata = new_data, type = "prob")

# Convert predicted_probs from list to matrix
predicted_probs_matrix <- do.call(rbind, predicted_probs)

# Pre-allocate a matrix to store the samples
random_samples_matrix <- matrix(0, nrow = nrow(predicted_probs_matrix), ncol = N)

# Draw random levels based on predicted probabilities for each observation
for(i in 1:nrow(predicted_probs_matrix)) {
  random_samples_matrix[i, ] <- sample(1:ncol(predicted_probs_matrix), 
                                       size = N, 
                                       replace = TRUE, 
                                       prob = predicted_probs_matrix[i, ])
}

tmp_data <- data_by_weeks[,c("mo", "tu", "we", "th", "fr", "sa", "q2")]
n_steps <- length(steps) - 1
points_bootstrap <- matrix(0, nrow = n_steps*ncol(random_samples_matrix), ncol = 3)
for(i in 1:ncol(random_samples_matrix)) {
  tmp_data$q2 <- random_samples_matrix[ ,i]
  tmp_data$os <- factor(tmp_data$q2, ordered = TRUE, levels = c(1, 2, 3, 4, 5))
  init_params <- kum_par # uniform distribution
  lower_bounds <- c(0.00001, 0.00001)
  opt_result <- optimx(init_params, objective_fun, method="L-BFGS-B", 
                       lower=lower_bounds, df = tmp_data, steps = steps)

  cat(i, kumaraswamy_weights(steps, opt_result$a, opt_result$b), "\n")
  # done protect against estimation error
  if (opt_result$kkt1 == TRUE & opt_result$kkt2 == TRUE) {
    print(kumaraswamy_weights(steps, opt_result$a, opt_result$b))
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 1] = i
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 2] = steps[2:(n_steps + 1)]
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 3] = kumaraswamy_weights(steps, opt_result$a, opt_result$b)
  }
  else {
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 1] = i
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 2] = steps[2:(n_steps + 1)]
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 3] = NA 
    }
}
points_bootstrap <- na.omit(points_bootstrap)
bootstrap_data <- tibble(rep = points_bootstrap[ ,1], step = points_bootstrap[ ,2], weight = points_bootstrap[ ,3])
saveRDS(bootstrap_data, file_name)

# specific data set ========= 2 ===========

#Here choose the csv, it can be with real p1204 or desired only
all_data <- read_csv("phase4_current_results_p1204.csv", col_types = "fnnnnnnnnnnnncc")
#Here choose for which data you want the model: 
#n<8 for all data, n==7 for only full weeks and n<7 for only unfull weeks
data_by_weeks_n <- all_data  %>% filter(n == 7)
#Here choose what content you want: 
#week < 26 for all content, week < 14 for nature only and week > 13 for slowmo only
data_by_weeks <- data_by_weeks_n %>% filter(week < 26)
#if you want to exclude some testers you can do this after this line
file_name = "bootstrap_n_is_7_week_less_26.rds"
data_by_weeks$os <- factor(data_by_weeks$q2, ordered = TRUE, 
                           levels = c(1, 2, 3, 4, 5))

best_AIC <- 10^9
opt_param <- 0
 
  init_params <- kum_par
  lower_bounds <- c(0.00001, 0.00001)
  opt_data <- data_by_weeks 
  
  opt_result <- optimx(init_params, objective_fun, method="L-BFGS-B", 
                       lower=lower_bounds, df = opt_data, steps = steps)
  
  best_params <- opt_result
  print(best_params)
  if (best_params$value < best_AIC)
  {
    best_AIC <- best_params$value
    opt_param <- best_params
  }

data_by_weeks$f_best_fun <- f_kum(as.matrix(
  data_by_weeks[,c("mo", "tu", "we", "th", "fr", "sa")]), 
  steps, opt_param$a, opt_param$b)
opt_model <- clm(os ~ f_best_fun, data = data_by_weeks, family = "binomial")
summary(opt_model)

ggplot(NULL, aes(steps[2:7], kumaraswamy_weights(steps, opt_param$a, opt_param$b))) + 
  geom_point()

# 1. Predict probabilities
new_data <- data.frame(f_best_fun = data_by_weeks$f_best_fun)
predicted_probs <- predict(opt_model, newdata = new_data, type = "prob")

# Convert predicted_probs from list to matrix
predicted_probs_matrix <- do.call(rbind, predicted_probs)

# Pre-allocate a matrix to store the samples
random_samples_matrix <- matrix(0, nrow = nrow(predicted_probs_matrix), ncol = N)

# Draw random levels based on predicted probabilities for each observation
for(i in 1:nrow(predicted_probs_matrix)) {
  random_samples_matrix[i, ] <- sample(1:ncol(predicted_probs_matrix), 
                                       size = N, 
                                       replace = TRUE, 
                                       prob = predicted_probs_matrix[i, ])
}

tmp_data <- data_by_weeks[,c("mo", "tu", "we", "th", "fr", "sa", "q2")]
n_steps <- length(steps) - 1
points_bootstrap <- matrix(0, nrow = n_steps*ncol(random_samples_matrix), ncol = 3)
for(i in 1:ncol(random_samples_matrix)) {
  tmp_data$q2 <- random_samples_matrix[ ,i]
  tmp_data$os <- factor(tmp_data$q2, ordered = TRUE, levels = c(1, 2, 3, 4, 5))
  init_params <- kum_par # uniform distribution
  lower_bounds <- c(0.00001, 0.00001)
  opt_result <- optimx(init_params, objective_fun, method="L-BFGS-B", 
                       lower=lower_bounds, df = tmp_data, steps = steps)

  cat(i, kumaraswamy_weights(steps, opt_result$a, opt_result$b), "\n")
  # done protect against estimation error
  if (opt_result$kkt1 == TRUE & opt_result$kkt2 == TRUE) {
    print(kumaraswamy_weights(steps, opt_result$a, opt_result$b))
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 1] = i
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 2] = steps[2:(n_steps + 1)]
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 3] = kumaraswamy_weights(steps, opt_result$a, opt_result$b)
  }
  else {
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 1] = i
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 2] = steps[2:(n_steps + 1)]
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 3] = NA 
    }
}
points_bootstrap <- na.omit(points_bootstrap)
bootstrap_data <- tibble(rep = points_bootstrap[ ,1], step = points_bootstrap[ ,2], weight = points_bootstrap[ ,3])
saveRDS(bootstrap_data, file_name)


# specific data set ========= 3 ===========

#Here choose the csv, it can be with real p1204 or desired only
all_data <- read_csv("phase4_current_results_p1204.csv", col_types = "fnnnnnnnnnnnncc")
#Here choose for which data you want the model: 
#n<8 for all data, n==7 for only full weeks and n<7 for only unfull weeks
data_by_weeks_n <- all_data  %>% filter(n < 7)
#Here choose what content you want: 
#week < 26 for all content, week < 14 for nature only and week > 13 for slowmo only
data_by_weeks <- data_by_weeks_n %>% filter(week < 26)
#if you want to exclude some testers you can do this after this line
file_name = "bootstrap_n_less_7_week_less_26.rds"
data_by_weeks$os <- factor(data_by_weeks$q2, ordered = TRUE, 
                           levels = c(1, 2, 3, 4, 5))

best_AIC <- 10^9
opt_param <- 0
 
  init_params <- kum_par
  lower_bounds <- c(0.00001, 0.00001)
  opt_data <- data_by_weeks 
  
  opt_result <- optimx(init_params, objective_fun, method="L-BFGS-B", 
                       lower=lower_bounds, df = opt_data, steps = steps)
  
  best_params <- opt_result
  print(best_params)
  if (best_params$value < best_AIC)
  {
    best_AIC <- best_params$value
    opt_param <- best_params
  }

data_by_weeks$f_best_fun <- f_kum(as.matrix(
  data_by_weeks[,c("mo", "tu", "we", "th", "fr", "sa")]), 
  steps, opt_param$a, opt_param$b)
opt_model <- clm(os ~ f_best_fun, data = data_by_weeks, family = "binomial")
summary(opt_model)

ggplot(NULL, aes(steps[2:7], kumaraswamy_weights(steps, opt_param$a, opt_param$b))) + 
  geom_point()

# 1. Predict probabilities
new_data <- data.frame(f_best_fun = data_by_weeks$f_best_fun)
predicted_probs <- predict(opt_model, newdata = new_data, type = "prob")

# Convert predicted_probs from list to matrix
predicted_probs_matrix <- do.call(rbind, predicted_probs)

# Pre-allocate a matrix to store the samples
random_samples_matrix <- matrix(0, nrow = nrow(predicted_probs_matrix), ncol = N)

# Draw random levels based on predicted probabilities for each observation
for(i in 1:nrow(predicted_probs_matrix)) {
  random_samples_matrix[i, ] <- sample(1:ncol(predicted_probs_matrix), 
                                       size = N, 
                                       replace = TRUE, 
                                       prob = predicted_probs_matrix[i, ])
}

tmp_data <- data_by_weeks[,c("mo", "tu", "we", "th", "fr", "sa", "q2")]
n_steps <- length(steps) - 1
points_bootstrap <- matrix(0, nrow = n_steps*ncol(random_samples_matrix), ncol = 3)
for(i in 1:ncol(random_samples_matrix)) {
  tmp_data$q2 <- random_samples_matrix[ ,i]
  tmp_data$os <- factor(tmp_data$q2, ordered = TRUE, levels = c(1, 2, 3, 4, 5))
  init_params <- kum_par # uniform distribution
  lower_bounds <- c(0.00001, 0.00001)
  opt_result <- optimx(init_params, objective_fun, method="L-BFGS-B", 
                       lower=lower_bounds, df = tmp_data, steps = steps)

  cat(i, kumaraswamy_weights(steps, opt_result$a, opt_result$b), "\n")
  # done protect against estimation error
  if (opt_result$kkt1 == TRUE & opt_result$kkt2 == TRUE) {
    print(kumaraswamy_weights(steps, opt_result$a, opt_result$b))
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 1] = i
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 2] = steps[2:(n_steps + 1)]
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 3] = kumaraswamy_weights(steps, opt_result$a, opt_result$b)
  }
  else {
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 1] = i
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 2] = steps[2:(n_steps + 1)]
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 3] = NA 
    }
}
points_bootstrap <- na.omit(points_bootstrap)
bootstrap_data <- tibble(rep = points_bootstrap[ ,1], step = points_bootstrap[ ,2], weight = points_bootstrap[ ,3])
saveRDS(bootstrap_data, file_name)


# specific data set ========= 4 ===========

#Here choose the csv, it can be with real p1204 or desired only
all_data <- read_csv("phase4_current_results_p1204.csv", col_types = "fnnnnnnnnnnnncc")
#Here choose for which data you want the model: 
#n<8 for all data, n==7 for only full weeks and n<7 for only unfull weeks
data_by_weeks_n <- all_data  %>% filter(n < 8)
#Here choose what content you want: 
#week < 26 for all content, week < 14 for nature only and week > 13 for slowmo only
data_by_weeks <- data_by_weeks_n %>% filter(week < 14)
#if you want to exclude some testers you can do this after this line
file_name = "bootstrap_n_less_8_week_less_14.rds"
data_by_weeks$os <- factor(data_by_weeks$q2, ordered = TRUE, 
                           levels = c(1, 2, 3, 4, 5))

best_AIC <- 10^9
opt_param <- 0
 
  init_params <- kum_par
  lower_bounds <- c(0.00001, 0.00001)
  opt_data <- data_by_weeks 
  
  opt_result <- optimx(init_params, objective_fun, method="L-BFGS-B", 
                       lower=lower_bounds, df = opt_data, steps = steps)
  
  best_params <- opt_result
  print(best_params)
  if (best_params$value < best_AIC)
  {
    best_AIC <- best_params$value
    opt_param <- best_params
  }

data_by_weeks$f_best_fun <- f_kum(as.matrix(
  data_by_weeks[,c("mo", "tu", "we", "th", "fr", "sa")]), 
  steps, opt_param$a, opt_param$b)
opt_model <- clm(os ~ f_best_fun, data = data_by_weeks, family = "binomial")
summary(opt_model)

ggplot(NULL, aes(steps[2:7], kumaraswamy_weights(steps, opt_param$a, opt_param$b))) + 
  geom_point()

# 1. Predict probabilities
new_data <- data.frame(f_best_fun = data_by_weeks$f_best_fun)
predicted_probs <- predict(opt_model, newdata = new_data, type = "prob")

# Convert predicted_probs from list to matrix
predicted_probs_matrix <- do.call(rbind, predicted_probs)

# Pre-allocate a matrix to store the samples
random_samples_matrix <- matrix(0, nrow = nrow(predicted_probs_matrix), ncol = N)

# Draw random levels based on predicted probabilities for each observation
for(i in 1:nrow(predicted_probs_matrix)) {
  random_samples_matrix[i, ] <- sample(1:ncol(predicted_probs_matrix), 
                                       size = N, 
                                       replace = TRUE, 
                                       prob = predicted_probs_matrix[i, ])
}

tmp_data <- data_by_weeks[,c("mo", "tu", "we", "th", "fr", "sa", "q2")]
n_steps <- length(steps) - 1
points_bootstrap <- matrix(0, nrow = n_steps*ncol(random_samples_matrix), ncol = 3)
for(i in 1:ncol(random_samples_matrix)) {
  tmp_data$q2 <- random_samples_matrix[ ,i]
  tmp_data$os <- factor(tmp_data$q2, ordered = TRUE, levels = c(1, 2, 3, 4, 5))
  init_params <- kum_par # uniform distribution
  lower_bounds <- c(0.00001, 0.00001)
  opt_result <- optimx(init_params, objective_fun, method="L-BFGS-B", 
                       lower=lower_bounds, df = tmp_data, steps = steps)

  cat(i, kumaraswamy_weights(steps, opt_result$a, opt_result$b), "\n")
  # done protect against estimation error
  if (opt_result$kkt1 == TRUE & opt_result$kkt2 == TRUE) {
    print(kumaraswamy_weights(steps, opt_result$a, opt_result$b))
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 1] = i
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 2] = steps[2:(n_steps + 1)]
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 3] = kumaraswamy_weights(steps, opt_result$a, opt_result$b)
  }
  else {
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 1] = i
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 2] = steps[2:(n_steps + 1)]
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 3] = NA 
    }
}
points_bootstrap <- na.omit(points_bootstrap)
bootstrap_data <- tibble(rep = points_bootstrap[ ,1], step = points_bootstrap[ ,2], weight = points_bootstrap[ ,3])
saveRDS(bootstrap_data, file_name)


# specific data set ========= 5 ===========

#Here choose the csv, it can be with real p1204 or desired only
all_data <- read_csv("phase4_current_results_p1204.csv", col_types = "fnnnnnnnnnnnncc")
#Here choose for which data you want the model: 
#n<8 for all data, n==7 for only full weeks and n<7 for only unfull weeks
data_by_weeks_n <- all_data  %>% filter(n < 8)
#Here choose what content you want: 
#week < 26 for all content, week < 14 for nature only and week > 13 for slowmo only
data_by_weeks <- data_by_weeks_n %>% filter(week > 13)
#if you want to exclude some testers you can do this after this line
file_name = "bootstrap_n_less_8_week_more_13.rds"
data_by_weeks$os <- factor(data_by_weeks$q2, ordered = TRUE, 
                           levels = c(1, 2, 3, 4, 5))

best_AIC <- 10^9
opt_param <- 0
 
  init_params <- kum_par
  lower_bounds <- c(0.00001, 0.00001)
  opt_data <- data_by_weeks 
  
  opt_result <- optimx(init_params, objective_fun, method="L-BFGS-B", 
                       lower=lower_bounds, df = opt_data, steps = steps)
  
  best_params <- opt_result
  print(best_params)
  if (best_params$value < best_AIC)
  {
    best_AIC <- best_params$value
    opt_param <- best_params
  }

data_by_weeks$f_best_fun <- f_kum(as.matrix(
  data_by_weeks[,c("mo", "tu", "we", "th", "fr", "sa")]), 
  steps, opt_param$a, opt_param$b)
opt_model <- clm(os ~ f_best_fun, data = data_by_weeks, family = "binomial")
summary(opt_model)

ggplot(NULL, aes(steps[2:7], kumaraswamy_weights(steps, opt_param$a, opt_param$b))) + 
  geom_point()

# 1. Predict probabilities
new_data <- data.frame(f_best_fun = data_by_weeks$f_best_fun)
predicted_probs <- predict(opt_model, newdata = new_data, type = "prob")

# Convert predicted_probs from list to matrix
predicted_probs_matrix <- do.call(rbind, predicted_probs)

# Pre-allocate a matrix to store the samples
random_samples_matrix <- matrix(0, nrow = nrow(predicted_probs_matrix), ncol = N)

# Draw random levels based on predicted probabilities for each observation
for(i in 1:nrow(predicted_probs_matrix)) {
  random_samples_matrix[i, ] <- sample(1:ncol(predicted_probs_matrix), 
                                       size = N, 
                                       replace = TRUE, 
                                       prob = predicted_probs_matrix[i, ])
}

tmp_data <- data_by_weeks[,c("mo", "tu", "we", "th", "fr", "sa", "q2")]
n_steps <- length(steps) - 1
points_bootstrap <- matrix(0, nrow = n_steps*ncol(random_samples_matrix), ncol = 3)
for(i in 1:ncol(random_samples_matrix)) {
  tmp_data$q2 <- random_samples_matrix[ ,i]
  tmp_data$os <- factor(tmp_data$q2, ordered = TRUE, levels = c(1, 2, 3, 4, 5))
  init_params <- kum_par # uniform distribution
  lower_bounds <- c(0.00001, 0.00001)
  opt_result <- optimx(init_params, objective_fun, method="L-BFGS-B", 
                       lower=lower_bounds, df = tmp_data, steps = steps)

  cat(i, kumaraswamy_weights(steps, opt_result$a, opt_result$b), "\n")
  # done protect against estimation error
  if (opt_result$kkt1 == TRUE & opt_result$kkt2 == TRUE) {
    print(kumaraswamy_weights(steps, opt_result$a, opt_result$b))
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 1] = i
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 2] = steps[2:(n_steps + 1)]
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 3] = kumaraswamy_weights(steps, opt_result$a, opt_result$b)
  }
  else {
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 1] = i
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 2] = steps[2:(n_steps + 1)]
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 3] = NA 
    }
}
points_bootstrap <- na.omit(points_bootstrap)
bootstrap_data <- tibble(rep = points_bootstrap[ ,1], step = points_bootstrap[ ,2], weight = points_bootstrap[ ,3])
saveRDS(bootstrap_data, file_name)


# specific data set ========= 6 ===========

#Here choose the csv, it can be with real p1204 or desired only
all_data <- read_csv("phase4_current_results_p1204.csv", col_types = "fnnnnnnnnnnnncc")
#Here choose for which data you want the model: 
#n<8 for all data, n==7 for only full weeks and n<7 for only unfull weeks
data_by_weeks_n <- all_data  %>% filter(n == 7)
#Here choose what content you want: 
#week < 26 for all content, week < 14 for nature only and week > 13 for slowmo only
data_by_weeks <- data_by_weeks_n %>% filter(week < 14)
#if you want to exclude some testers you can do this after this line
file_name = "bootstrap_n_is_7_week_less_14.rds"
data_by_weeks$os <- factor(data_by_weeks$q2, ordered = TRUE, 
                           levels = c(1, 2, 3, 4, 5))

best_AIC <- 10^9
opt_param <- 0
 
  init_params <- kum_par
  lower_bounds <- c(0.00001, 0.00001)
  opt_data <- data_by_weeks 
  
  opt_result <- optimx(init_params, objective_fun, method="L-BFGS-B", 
                       lower=lower_bounds, df = opt_data, steps = steps)
  
  best_params <- opt_result
  print(best_params)
  if (best_params$value < best_AIC)
  {
    best_AIC <- best_params$value
    opt_param <- best_params
  }

data_by_weeks$f_best_fun <- f_kum(as.matrix(
  data_by_weeks[,c("mo", "tu", "we", "th", "fr", "sa")]), 
  steps, opt_param$a, opt_param$b)
opt_model <- clm(os ~ f_best_fun, data = data_by_weeks, family = "binomial")
summary(opt_model)

ggplot(NULL, aes(steps[2:7], kumaraswamy_weights(steps, opt_param$a, opt_param$b))) + 
  geom_point()

# 1. Predict probabilities
new_data <- data.frame(f_best_fun = data_by_weeks$f_best_fun)
predicted_probs <- predict(opt_model, newdata = new_data, type = "prob")

# Convert predicted_probs from list to matrix
predicted_probs_matrix <- do.call(rbind, predicted_probs)

# Pre-allocate a matrix to store the samples
random_samples_matrix <- matrix(0, nrow = nrow(predicted_probs_matrix), ncol = N)

# Draw random levels based on predicted probabilities for each observation
for(i in 1:nrow(predicted_probs_matrix)) {
  random_samples_matrix[i, ] <- sample(1:ncol(predicted_probs_matrix), 
                                       size = N, 
                                       replace = TRUE, 
                                       prob = predicted_probs_matrix[i, ])
}

tmp_data <- data_by_weeks[,c("mo", "tu", "we", "th", "fr", "sa", "q2")]
n_steps <- length(steps) - 1
points_bootstrap <- matrix(0, nrow = n_steps*ncol(random_samples_matrix), ncol = 3)
for(i in 1:ncol(random_samples_matrix)) {
  tmp_data$q2 <- random_samples_matrix[ ,i]
  tmp_data$os <- factor(tmp_data$q2, ordered = TRUE, levels = c(1, 2, 3, 4, 5))
  init_params <- kum_par # uniform distribution
  lower_bounds <- c(0.00001, 0.00001)
  opt_result <- optimx(init_params, objective_fun, method="L-BFGS-B", 
                       lower=lower_bounds, df = tmp_data, steps = steps)

  cat(i, kumaraswamy_weights(steps, opt_result$a, opt_result$b), "\n")
  # done protect against estimation error
  if (opt_result$kkt1 == TRUE & opt_result$kkt2 == TRUE) {
    print(kumaraswamy_weights(steps, opt_result$a, opt_result$b))
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 1] = i
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 2] = steps[2:(n_steps + 1)]
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 3] = kumaraswamy_weights(steps, opt_result$a, opt_result$b)
  }
  else {
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 1] = i
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 2] = steps[2:(n_steps + 1)]
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 3] = NA 
    }
}
points_bootstrap <- na.omit(points_bootstrap)
bootstrap_data <- tibble(rep = points_bootstrap[ ,1], step = points_bootstrap[ ,2], weight = points_bootstrap[ ,3])
saveRDS(bootstrap_data, file_name)


# specific data set ========= 7 ===========

#Here choose the csv, it can be with real p1204 or desired only
all_data <- read_csv("phase4_current_results_p1204.csv", col_types = "fnnnnnnnnnnnncc")
#Here choose for which data you want the model: 
#n<8 for all data, n==7 for only full weeks and n<7 for only unfull weeks
data_by_weeks_n <- all_data  %>% filter(n == 7)
#Here choose what content you want: 
#week < 26 for all content, week < 14 for nature only and week > 13 for slowmo only
data_by_weeks <- data_by_weeks_n %>% filter(week > 13)
#if you want to exclude some testers you can do this after this line
file_name = "bootstrap_n_is_7_week_more_13.rds"
data_by_weeks$os <- factor(data_by_weeks$q2, ordered = TRUE, 
                           levels = c(1, 2, 3, 4, 5))

best_AIC <- 10^9
opt_param <- 0
 
  init_params <- kum_par
  lower_bounds <- c(0.00001, 0.00001)
  opt_data <- data_by_weeks 
  
  opt_result <- optimx(init_params, objective_fun, method="L-BFGS-B", 
                       lower=lower_bounds, df = opt_data, steps = steps)
  
  best_params <- opt_result
  print(best_params)
  if (best_params$value < best_AIC)
  {
    best_AIC <- best_params$value
    opt_param <- best_params
  }

data_by_weeks$f_best_fun <- f_kum(as.matrix(
  data_by_weeks[,c("mo", "tu", "we", "th", "fr", "sa")]), 
  steps, opt_param$a, opt_param$b)
opt_model <- clm(os ~ f_best_fun, data = data_by_weeks, family = "binomial")
summary(opt_model)

ggplot(NULL, aes(steps[2:7], kumaraswamy_weights(steps, opt_param$a, opt_param$b))) + 
  geom_point()

# 1. Predict probabilities
new_data <- data.frame(f_best_fun = data_by_weeks$f_best_fun)
predicted_probs <- predict(opt_model, newdata = new_data, type = "prob")

# Convert predicted_probs from list to matrix
predicted_probs_matrix <- do.call(rbind, predicted_probs)

# Pre-allocate a matrix to store the samples
random_samples_matrix <- matrix(0, nrow = nrow(predicted_probs_matrix), ncol = N)

# Draw random levels based on predicted probabilities for each observation
for(i in 1:nrow(predicted_probs_matrix)) {
  random_samples_matrix[i, ] <- sample(1:ncol(predicted_probs_matrix), 
                                       size = N, 
                                       replace = TRUE, 
                                       prob = predicted_probs_matrix[i, ])
}

tmp_data <- data_by_weeks[,c("mo", "tu", "we", "th", "fr", "sa", "q2")]
n_steps <- length(steps) - 1
points_bootstrap <- matrix(0, nrow = n_steps*ncol(random_samples_matrix), ncol = 3)
for(i in 1:ncol(random_samples_matrix)) {
  tmp_data$q2 <- random_samples_matrix[ ,i]
  tmp_data$os <- factor(tmp_data$q2, ordered = TRUE, levels = c(1, 2, 3, 4, 5))
  init_params <- kum_par # uniform distribution
  lower_bounds <- c(0.00001, 0.00001)
  opt_result <- optimx(init_params, objective_fun, method="L-BFGS-B", 
                       lower=lower_bounds, df = tmp_data, steps = steps)
  cat(i, kumaraswamy_weights(steps, opt_result$a, opt_result$b), "\n")
  # done protect against estimation error
  if (opt_result$kkt1 == TRUE & opt_result$kkt2 == TRUE) {
    print(kumaraswamy_weights(steps, opt_result$a, opt_result$b))
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 1] = i
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 2] = steps[2:(n_steps + 1)]
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 3] = kumaraswamy_weights(steps, opt_result$a, opt_result$b)
  }
  else {
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 1] = i
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 2] = steps[2:(n_steps + 1)]
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 3] = NA 
    }
}
points_bootstrap <- na.omit(points_bootstrap)
bootstrap_data <- tibble(rep = points_bootstrap[ ,1], step = points_bootstrap[ ,2], weight = points_bootstrap[ ,3])
saveRDS(bootstrap_data, file_name)


# specific data set ========= 8 ===========

#Here choose the csv, it can be with real p1204 or desired only
all_data <- read_csv("phase4_current_results_p1204.csv", col_types = "fnnnnnnnnnnnncc")
#Here choose for which data you want the model: 
#n<8 for all data, n==7 for only full weeks and n<7 for only unfull weeks
data_by_weeks_n <- all_data  %>% filter(n < 7)
#Here choose what content you want: 
#week < 26 for all content, week < 14 for nature only and week > 13 for slowmo only
data_by_weeks <- data_by_weeks_n %>% filter(week < 14)
#if you want to exclude some testers you can do this after this line
file_name = "bootstrap_n_less_7_week_less_14.rds"
data_by_weeks$os <- factor(data_by_weeks$q2, ordered = TRUE, 
                           levels = c(1, 2, 3, 4, 5))

best_AIC <- 10^9
opt_param <- 0
 
  init_params <- kum_par
  lower_bounds <- c(0.00001, 0.00001)
  opt_data <- data_by_weeks 
  
  opt_result <- optimx(init_params, objective_fun, method="L-BFGS-B", 
                       lower=lower_bounds, df = opt_data, steps = steps)
  
  best_params <- opt_result
  print(best_params)
  if (best_params$value < best_AIC)
  {
    best_AIC <- best_params$value
    opt_param <- best_params
  }

data_by_weeks$f_best_fun <- f_kum(as.matrix(
  data_by_weeks[,c("mo", "tu", "we", "th", "fr", "sa")]), 
  steps, opt_param$a, opt_param$b)
opt_model <- clm(os ~ f_best_fun, data = data_by_weeks, family = "binomial")
summary(opt_model)

ggplot(NULL, aes(steps[2:7], kumaraswamy_weights(steps, opt_param$a, opt_param$b))) + 
  geom_point()

# 1. Predict probabilities
new_data <- data.frame(f_best_fun = data_by_weeks$f_best_fun)
predicted_probs <- predict(opt_model, newdata = new_data, type = "prob")

# Convert predicted_probs from list to matrix
predicted_probs_matrix <- do.call(rbind, predicted_probs)

# Pre-allocate a matrix to store the samples
random_samples_matrix <- matrix(0, nrow = nrow(predicted_probs_matrix), ncol = N)

# Draw random levels based on predicted probabilities for each observation
for(i in 1:nrow(predicted_probs_matrix)) {
  random_samples_matrix[i, ] <- sample(1:ncol(predicted_probs_matrix), 
                                       size = N, 
                                       replace = TRUE, 
                                       prob = predicted_probs_matrix[i, ])
}

tmp_data <- data_by_weeks[,c("mo", "tu", "we", "th", "fr", "sa", "q2")]
n_steps <- length(steps) - 1
points_bootstrap <- matrix(0, nrow = n_steps*ncol(random_samples_matrix), ncol = 3)
for(i in 1:ncol(random_samples_matrix)) {
  tmp_data$q2 <- random_samples_matrix[ ,i]
  tmp_data$os <- factor(tmp_data$q2, ordered = TRUE, levels = c(1, 2, 3, 4, 5))
  init_params <- kum_par # uniform distribution
  lower_bounds <- c(0.00001, 0.00001)
  opt_result <- optimx(init_params, objective_fun, method="L-BFGS-B", 
                       lower=lower_bounds, df = tmp_data, steps = steps)
  cat(i, kumaraswamy_weights(steps, opt_result$a, opt_result$b), "\n")
  # done protect against estimation error
  if (opt_result$kkt1 == TRUE & opt_result$kkt2 == TRUE) {
    print(kumaraswamy_weights(steps, opt_result$a, opt_result$b))
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 1] = i
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 2] = steps[2:(n_steps + 1)]
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 3] = kumaraswamy_weights(steps, opt_result$a, opt_result$b)
  }
  else {
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 1] = i
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 2] = steps[2:(n_steps + 1)]
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 3] = NA 
    }
}
points_bootstrap <- na.omit(points_bootstrap)
bootstrap_data <- tibble(rep = points_bootstrap[ ,1], step = points_bootstrap[ ,2], weight = points_bootstrap[ ,3])
saveRDS(bootstrap_data, file_name)


# specific data set ========= 9 ===========

#Here choose the csv, it can be with real p1204 or desired only
all_data <- read_csv("phase4_current_results_p1204.csv", col_types = "fnnnnnnnnnnnncc")
#Here choose for which data you want the model: 
#n<8 for all data, n==7 for only full weeks and n<7 for only unfull weeks
data_by_weeks_n <- all_data  %>% filter(n < 7)
#Here choose what content you want: 
#week < 26 for all content, week < 14 for nature only and week > 13 for slowmo only
data_by_weeks <- data_by_weeks_n %>% filter(week > 13)
#if you want to exclude some testers you can do this after this line
file_name = "bootstrap_n_less_7_week_more_13.rds"
data_by_weeks$os <- factor(data_by_weeks$q2, ordered = TRUE, 
                           levels = c(1, 2, 3, 4, 5))

best_AIC <- 10^9
opt_param <- 0
 
  init_params <- kum_par
  lower_bounds <- c(0.00001, 0.00001)
  opt_data <- data_by_weeks 
  
  opt_result <- optimx(init_params, objective_fun, method="L-BFGS-B", 
                       lower=lower_bounds, df = opt_data, steps = steps)
  
  best_params <- opt_result
  print(best_params)
  if (best_params$value < best_AIC)
  {
    best_AIC <- best_params$value
    opt_param <- best_params
  }

data_by_weeks$f_best_fun <- f_kum(as.matrix(
  data_by_weeks[,c("mo", "tu", "we", "th", "fr", "sa")]), 
  steps, opt_param$a, opt_param$b)
opt_model <- clm(os ~ f_best_fun, data = data_by_weeks, family = "binomial")
summary(opt_model)

ggplot(NULL, aes(steps[2:7], kumaraswamy_weights(steps, opt_param$a, opt_param$b))) + 
  geom_point()

# 1. Predict probabilities
new_data <- data.frame(f_best_fun = data_by_weeks$f_best_fun)
predicted_probs <- predict(opt_model, newdata = new_data, type = "prob")

# Convert predicted_probs from list to matrix
predicted_probs_matrix <- do.call(rbind, predicted_probs)

# Pre-allocate a matrix to store the samples
random_samples_matrix <- matrix(0, nrow = nrow(predicted_probs_matrix), ncol = N)

# Draw random levels based on predicted probabilities for each observation
for(i in 1:nrow(predicted_probs_matrix)) {
  random_samples_matrix[i, ] <- sample(1:ncol(predicted_probs_matrix), 
                                       size = N, 
                                       replace = TRUE, 
                                       prob = predicted_probs_matrix[i, ])
}

tmp_data <- data_by_weeks[,c("mo", "tu", "we", "th", "fr", "sa", "q2")]
n_steps <- length(steps) - 1
points_bootstrap <- matrix(0, nrow = n_steps*ncol(random_samples_matrix), ncol = 3)
for(i in 1:ncol(random_samples_matrix)) {
  tmp_data$q2 <- random_samples_matrix[ ,i]
  tmp_data$os <- factor(tmp_data$q2, ordered = TRUE, levels = c(1, 2, 3, 4, 5))
  init_params <- kum_par # uniform distribution
  lower_bounds <- c(0.00001, 0.00001)
  opt_result <- optimx(init_params, objective_fun, method="L-BFGS-B", 
                       lower=lower_bounds, df = tmp_data, steps = steps)
  cat(i, kumaraswamy_weights(steps, opt_result$a, opt_result$b), "\n")
  # done protect against estimation error (może if $kkt1 to proceed)
  if (opt_result$kkt1 == TRUE & opt_result$kkt2 == TRUE) {
    print(kumaraswamy_weights(steps, opt_result$a, opt_result$b))
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 1] = i
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 2] = steps[2:(n_steps + 1)]
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 3] = kumaraswamy_weights(steps, opt_result$a, opt_result$b)
  }
  else {
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 1] = i
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 2] = steps[2:(n_steps + 1)]
    points_bootstrap[((i - 1)*n_steps + 1):(i*n_steps), 3] = NA 
    }
}
points_bootstrap <- na.omit(points_bootstrap)
bootstrap_data <- tibble(rep = points_bootstrap[ ,1], step = points_bootstrap[ ,2], weight = points_bootstrap[ ,3])
saveRDS(bootstrap_data, file_name)
