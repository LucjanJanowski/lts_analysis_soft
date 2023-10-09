library(tidyverse)
library(ggpubr)
library(ordinal)
library(optimx)
library(stats)
library(data.table)
library(boot)

#Here choose the csv, it can be with real p1204 or desired only
all_data <- read_csv("phase4_current_results_p1204.csv", col_types = "ffnnnnnnnnnnncc")
#Here choose for which data you want the model: 
#n<8 for all data, n==7 for only full weeks and n<7 for only unfull weeks
data_by_weeks_n <- all_data  %>% filter(n == 7)
#Here choose what content you want: 
#n < 26 for all content, n < 14 for nature only and n > 13 for slowmo only
data_by_weeks <- data_by_weeks_n %>% filter(n < 26)
#if you want to exclude some testers you can do this after this line

# Clean data (tester excluding)  ============================

#data_by_weeks <- data_by_weeks[!(data_by_weeks$external_id %in% c("f1308", "bb8d0", "343ab", "9d7d1", "3fe63", "57ea9")), ]

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

f_mean <- function(x) {
  tmp <- x[,1]
  tmp[is.na(x[,1])] <- 0
  norm <- rep(1, nrow(x))
  norm[is.na(x[,1])] <- 0
  for (i in c(2:6)){
    tmp_norm <- rep(1, nrow(x))
    tmp_norm[is.na(x[,i])] <- 0
    norm <- norm + tmp_norm
    tmp_na <- x[,i]
    tmp_na[is.na(tmp_na)] <- 0
    tmp <- tmp + tmp_na
  }
  return(tmp / norm)
}

f_min <- function(x) {
  tmp <- x[,1]
  tmp[is.na(x[,1])] <- Inf  # Initialize with positive infinity
  for (i in c(2:6)){
    tmp_na <- x[,i]
    tmp_na[is.na(tmp_na)] <- Inf  # Treat missing values as positive infinity
    tmp <- pmin(tmp, tmp_na)  # Calculate the element-wise minimum
  }
  tmp[tmp == Inf] <- NA  # Replace positive infinity with NA
  return(tmp)
}

data_by_weeks$f_mean = f_mean(as.matrix(data_by_weeks[,c("mo", "tu", "we", "th", "fr", "sa")]))
data_by_weeks$f_min = f_min(as.matrix(data_by_weeks[,c("mo", "tu", "we", "th", "fr", "sa")]))
data_by_weeks$os <- factor(data_by_weeks$q2, ordered = TRUE, 
                           levels = c(1, 2, 3, 4, 5))

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

best_AIC <- 10^9
opt_param <- 0
for (i in 1:nrow(kum_par)){
  init_params <- kum_par[i,]
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
N <- 100
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
  init_params <- kum_par[3,] # uniform distribution
  lower_bounds <- c(0.00001, 0.00001)
  opt_result <- optimx(init_params, objective_fun, method="L-BFGS-B", 
                       lower=lower_bounds, df = tmp_data, steps = steps)
  # TODO protect against estimation error
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
# Part2: change here to bootstrap_data_ex for plot without excluded testers 
points_bootstrap <- na.omit(points_bootstrap)
bootstrap_data <- tibble(rep = points_bootstrap[ ,1], step = points_bootstrap[ ,2], weight = points_bootstrap[ ,3])
ggplot(bootstrap_data, aes(step, weight)) + geom_point(alpha = 0.1)

ggplot() + geom_point(data = bootstrap_data, aes(x = step, y = weight), alpha = 0.1) +
  geom_point(data = bootstrap_data_ex, aes(x = step, y = weight), alpha = 0.1, color = "red")

summarized_data <- bootstrap_data %>%
  group_by(step) %>%
  summarise(mean_weight = mean(weight))
summarized_data_ex <- bootstrap_data_ex %>%
  group_by(step) %>%
  summarise(mean_weight = mean(weight))

ggplot() + geom_point(data = summarized_data, aes(x = step, y = mean_weight, color = "all"), alpha = 0.8) +
  geom_point(data = summarized_data_ex, aes(x = step, y = mean_weight, color = "clean"), alpha = 0.8) 
  

bootstrap_data$get_in <- 1
setDT(bootstrap_data)
# Po wielu próbach nie udało mi się zrobić odrzucania 5% z każdego stepu.
# Szukanie rozwiązania, które będzie usuwało wszystkie rep do którego należał usunięty step prowadzi do nieskończonej pętli
# W R jest też biblioteka boot, która ma wbudowaną funkcje liczenia CI - czemu nie możemy jej zastosować?
# Zostawiam póki co ten case nierozwiązany, do poruszenia na spotkaniu



# Initialize a data table and compute the abs_dif within each step group

bootstrap_data[, abs_dif := abs(weight - mean(weight)), by = step]

bootstrap_function <- function(data, indices) {
  steps <- c(0, 1/6, 2/6, 3/6, 4/6, 5/6, 6/6)
  x1 <- data[indices, 1]
  x2 <- data[indices, 2]
  return(kumaraswamy_weights(steps, x1, x2))  
}

desired_length <- nrow(bootstrap_data) * 0.95
bootstrap_data_ra <- bootstrap_data[get_in == 1]

set.seed(123)  # Set a seed for reproducibility
bootstrap_results <- boot(data = opt_result, statistic = bootstrap_function, R = 1000)

boot.ci(boot.out = bootstrap_results, 
        type = c("norm", "basic",
                 "perc", "bca"))


#this solution cannot be found
#while (nrow(bootstrap_data_ra) > desired_length) {
  # Filter by get_in, compute abs_dif and remove rows with max abs_dif rep
  #for (x in unique(bootstrap_data$step)){
    #print(x)
    #bootstrap_data_ra <- bootstrap_data_ra[get_in == 1]
    #bootstrap_data_ra[, abs_dif := abs(weight - mean(weight)), by = step]
    #bootstrap_data_tmp <- bootstrap_data %>% filter(step == x)
    #reps_with_max_abs_dif <- bootstrap_data_tmp[abs_dif == max(abs_dif), unique(rep)]
    #bootstrap_data_ra[rep %in% reps_with_max_abs_dif, get_in := 0]
  #}
#}


#bootstrap_data_ra <- as.data.frame(bootstrap_data_ra)
#ggplot(bootstrap_data_ra, aes(step, weight, group = rep)) + geom_point(size = 1, alpha = 0.03)

# If you need the result as a data.frame
result <- as.data.frame(result)
ggplot(result, aes(step, weight, group = rep)) + geom_point(size = 1, alpha = 0.03)
ggplot(result, aes(step, weight, group = rep)) + geom_line(size = 1, alpha = 0.03)



glz_data <- all_data %>%
  filter(n == 7)
glz_data$os <- factor(glz_data$q2, ordered = TRUE,levels = c(1, 2, 3, 4, 5))
model <- clm(os ~ mo + tu + we + th + fr + sa, data = glz_data, family = "binomial")  
AIC(model)
summary(model)

glz_data %<>%
  mutate(motu = (mo + tu)/2) %>%
  mutate(weth = (we + th)/2) %>%
  mutate(frsa = (fr + sa)/2)
model <- clm(os ~ motu + weth + frsa, data = glz_data, family = "binomial")  
AIC(model)
summary(model)

comparison <- data_by_weeks %>%
  group_by(week) %>%
  dplyr::summarize(mos = mean(q2), f_best_fun = mean(f_best_fun), f_mean = mean(f_mean), f_min = mean(f_min), n = n())

  ggscatter(data = comparison, x = "f_best_fun", y = "mos", 
            add = "reg.line",  # Add regression line
            add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE) + 
  stat_cor(method = "pearson", label.x = 2.5, label.y = 5)  
    ggscatter(data = comparison, x = "f_mean", y = "mos",
              add = "reg.line",  # Add regression line
              add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
              conf.int = TRUE) + 
    stat_cor(method = "pearson", label.x = 2.8, label.y = 5)
    ggscatter(data = comparison, x = "f_min", y = "mos",
              add = "reg.line",  # Add regression line
              add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
              conf.int = TRUE) + 
    stat_cor(method = "pearson", label.x = 2, label.y = 5) 


  