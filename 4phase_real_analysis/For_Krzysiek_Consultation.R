library(tidyverse)
library(ggpubr)
library(ordinal)
library(optimx)

# reading data ===================================
setwd("./4phase_real_analysis/")
data_by_weeks_1204 <- read_csv("phase4_current_results_p1204.csv", col_types = "ffnnnnnnnnnnncc")
data_by_weeks <- read_csv("phase4_current_results.csv", col_types = "ffnnnnnnnnnnncc")

# Different classes of weighting functions ============================
kumaraswa_pdf <- function(x, a, b){
  a * b * x^(a - 1) * (1 - x^a)^(b - 1)
}

kumaraswa_cdf <- function(x, a, b){
  1 - (1 - x^a)^b
}

kumaraswa_rev_cdf <- function(x, a, b){
  (1 - x^a)^b
}

x <- seq(0, 1, by=0.001)
ggplot(NULL, aes(x, kumaraswa_pdf(x, 2, 200))) + geom_line()
ggplot(NULL, aes(x, kumaraswa_cdf(x, 2, 2))) + geom_line()
ggplot(NULL, aes(x, kumaraswa_rev_cdf(x, 0.5, 2))) + geom_line()

# Modeling - simple mean ===========================
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

data_by_weeks$f_mean = f_mean(as.matrix(data_by_weeks[,c("mo", "tu", "we", "th", "fr", "sa")]))
data_by_weeks_1204$f_mean = f_mean(as.matrix(data_by_weeks_1204[,c("mo", "tu", "we", "th", "fr", "sa")]))

data_by_weeks %>%
  filter(n == 7) %>%
  group_by(week) %>% 
  dplyr::summarize(n_votes = n(), mos_real = mean(q2, na.rm = TRUE), 
                   mos_k = mean(f_mean, na.rm = TRUE), 
                   sd_real = sd(q2, na.rm = TRUE), 
                   sd_k = sd(f_mean, na.rm = TRUE)) %>%
  ggplot(aes(week, mos_real)) + 
  geom_errorbar(aes(ymin = mos_real - 1.96*sd_real/sqrt(n_votes), 
                    ymax = mos_real + 1.96*sd_real/sqrt(n_votes))) + 
  geom_point(aes(week, mos_k), color = 'blue')
data_by_weeks %>%
  select(-q4) %>%
  replace(is.na(.), 0) %>%
  mutate(week_private = mo*10^5+tu*10^4+we*10^3+th*10^2+fr*10+sa) %>%
  group_by(week_private) %>% 
  dplyr::summarize(n_votes = n(), mos_real = mean(q2, na.rm = TRUE), 
                   mos_k = mean(f_mean, na.rm = TRUE), 
                   sd_real = sd(q2, na.rm = TRUE), 
                   sd_k = sd(f_mean, na.rm = TRUE)) %>%
  ggplot(aes(mos_k, mos_real)) + 
  geom_errorbar(aes(ymin = mos_real - 1.96*sd_real/sqrt(n_votes), 
                    ymax = mos_real + 1.96*sd_real/sqrt(n_votes)))
data_by_weeks %>%
  select(-q4) %>%
  replace(is.na(.), 0) %>%
  mutate(week_private = mo*10^5+tu*10^4+we*10^3+th*10^2+fr*10+sa) %>%
  group_by(week_private) %>% 
  dplyr::summarize(n_votes = n(), mos_real = mean(q2, na.rm = TRUE), 
                   mos_k = mean(f_mean, na.rm = TRUE), 
                   sd_real = sd(q2, na.rm = TRUE), 
                   sd_k = sd(f_mean, na.rm = TRUE)) %>%
  ggplot(aes(mos_k, mos_real, color = log10(n_votes))) + 
  geom_point() +
  geom_smooth(colour = "green", alpha = 0.1) +
  geom_smooth(method = "lm") +
  stat_cor()

data_by_weeks_1204 %>%
  filter(n == 7) %>%
  group_by(week) %>% 
  dplyr::summarize(n_votes = n(), mos_real = mean(q2, na.rm = TRUE), 
                   mos_k = mean(f_mean, na.rm = TRUE), 
                   sd_real = sd(q2, na.rm = TRUE), 
                   sd_k = sd(f_mean, na.rm = TRUE)) %>%
  ggplot(aes(week, mos_real)) + 
  geom_errorbar(aes(ymin = mos_real - 1.96*sd_real/sqrt(n_votes), 
                    ymax = mos_real + 1.96*sd_real/sqrt(n_votes))) + 
  geom_point(aes(week, mos_k), color = 'blue')
data_by_weeks_1204 %>%
  select(-q4) %>%
  replace(is.na(.), 0) %>%
  mutate(week_private = mo*10^5+tu*10^4+we*10^3+th*10^2+fr*10+sa) %>%
  group_by(week_private) %>% 
  dplyr::summarize(n_votes = n(), mos_real = mean(q2, na.rm = TRUE), 
                   mos_k = mean(f_mean, na.rm = TRUE), 
                   sd_real = sd(q2, na.rm = TRUE), 
                   sd_k = sd(f_mean, na.rm = TRUE)) %>%
  ggplot(aes(mos_k, mos_real)) + 
  geom_errorbar(aes(ymin = mos_real - 1.96*sd_real/sqrt(n_votes), 
                    ymax = mos_real + 1.96*sd_real/sqrt(n_votes)))
data_by_weeks_1204 %>%
  select(-q4) %>%
  replace(is.na(.), 0) %>%
  mutate(week_private = mo*10^5+tu*10^4+we*10^3+th*10^2+fr*10+sa) %>%
  group_by(week_private) %>% 
  dplyr::summarize(n_votes = n(), mos_real = mean(q2, na.rm = TRUE), 
                   mos_k = mean(f_mean, na.rm = TRUE), 
                   sd_real = sd(q2, na.rm = TRUE), 
                   sd_k = sd(f_mean, na.rm = TRUE)) %>%
  ggplot(aes(mos_k, mos_real, color = log10(n_votes))) + 
  geom_point() +
  geom_smooth(colour = "green", alpha = 0.1) +
  geom_smooth(method = "lm") +
  stat_cor()




f_lin_last <- function(x) {
  norm <- rep(1, nrow(x))
  norm[is.na(x[,1])] <- 0
  tmp <- x[,1]
  tmp[is.na(x[,1])] <- 0
  tmp <- tmp * norm
  for (i in c(2:6)){
    tmp_norm <- rep(i, nrow(x))
    tmp_norm[is.na(x[,i])] <- 0
    norm <- norm + tmp_norm
    tmp_na <- x[,i]
    tmp_na[is.na(tmp_na)] <- 0
    tmp <- tmp + tmp_na * tmp_norm
  }
  return(tmp / norm)
}

f_lin_first <- function(x) {
  norm <- rep(6, nrow(x))
  norm[is.na(x[,1])] <- 0
  tmp <- x[,1]
  tmp[is.na(x[,1])] <- 0
  tmp <- tmp * norm
  for (i in c(2:6)){
    tmp_norm <- rep(7-i, nrow(x))
    tmp_norm[is.na(x[,i])] <- 0
    norm <- norm + tmp_norm
    tmp_na <- x[,i]
    tmp_na[is.na(tmp_na)] <- 0
    tmp <- tmp + tmp_na * tmp_norm
  }
  return(tmp / norm)
}

f_lin_ends <- function(x) {
  norm <- rep(3, nrow(x))
  norm[is.na(x[,1])] <- 0
  tmp <- x[,1]
  tmp[is.na(x[,1])] <- 0
  tmp <- tmp * norm
  for (i in c(2:6)){
    tmp_norm <- rep(ceiling(abs(3.5 - i)), nrow(x))
    tmp_norm[is.na(x[,i])] <- 0
    norm <- norm + tmp_norm
    tmp_na <- x[,i]
    tmp_na[is.na(tmp_na)] <- 0
    tmp <- tmp + tmp_na * tmp_norm
  }
  return(tmp / norm)
}



# Modeling =========================


data_by_weeks$f_lin_last = f_lin_last(as.matrix(data_by_weeks[,c("mo", "tu", "we", "th", "fr", "sa")]))

data_by_weeks$f_lin_first = f_lin_first(as.matrix(data_by_weeks[,c("mo", "tu", "we", "th", "fr", "sa")]))
data_by_weeks$f_lin_ends = f_lin_ends(as.matrix(data_by_weeks[,c("mo", "tu", "we", "th", "fr", "sa")]))

data_by_weeks$res_mean <- data_by_weeks$f_mean - data_by_weeks$q2
data_by_weeks$res_last <-data_by_weeks$f_lin_last - data_by_weeks$q2
data_by_weeks$res_first <-data_by_weeks$f_lin_first - data_by_weeks$q2
data_by_weeks$res_ends <-data_by_weeks$f_lin_ends - data_by_weeks$q2



d_mos_full_k <- data_by_weeks %>% filter(n == 7)  %>% 
  group_by(week) %>% 
  dplyr::summarize(n_votes = n(), mos_real = mean(q2, na.rm = TRUE), mos_k = mean(f_param, na.rm = TRUE), sd_real = sd(q2, na.rm = TRUE), sd_k = sd(f_param, na.rm = TRUE))

d_mos_full_k$diff <- d_mos_full_k$mos_real - d_mos_full_k$mos_k




data_by_weeks %>%
  filter(n == 7) %>%
  mutate(week_private = mo*10^5+tu*10^4+we*10^3+th*10^2+fr*10+sa) %>%
  group_by(week_private) %>%
  dplyr::summarize(mos = mean(q2), f_mean = mean(f_mean)) %>%
  ggscatter(x = "f_mean", y = "mos",
            add = "reg.line",  # Add regressin line
            add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE # Add confidence interval
  ) + 
  stat_cor(method = "pearson", label.x = 2.5, label.y = 5)

data_by_weeks %>%
  filter(n == 7) %>%
  mutate(week_private = mo*10^5+tu*10^4+we*10^3+th*10^2+fr*10+sa) %>%
  group_by(week_private) %>%
  dplyr::summarize(mos = mean(q2), f_lin_last = mean(f_lin_last)) %>%
  ggscatter(x = "f_lin_last", y = "mos",
            add = "reg.line",  # Add regressin line
            add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE # Add confidence interval
  ) + 
  stat_cor(method = "pearson", label.x = 2.5, label.y = 5)

data_by_weeks %>%
  filter(n == 7) %>%
  mutate(week_private = mo*10^5+tu*10^4+we*10^3+th*10^2+fr*10+sa) %>%
  group_by(week_private) %>%
  dplyr::summarize(mos = mean(q2), f_lin_first = mean(f_lin_first)) %>%
  ggscatter(x = "f_lin_first", y = "mos",
            add = "reg.line",  # Add regressin line
            add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE # Add confidence interval
  ) + 
  stat_cor(method = "pearson", label.x = 2.5, label.y = 5)

data_by_weeks %>%
  filter(n == 7) %>%
  mutate(week_private = mo*10^5+tu*10^4+we*10^3+th*10^2+fr*10+sa) %>%
  group_by(week_private) %>%
  dplyr::summarize(mos = mean(q2), f_lin_ends = mean(f_lin_ends)) %>%
  ggscatter(x = "f_lin_ends", y = "mos",
            add = "reg.line",  # Add regressin line
            add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE # Add confidence interval
  ) + 
  stat_cor(method = "pearson", label.x = 2.5, label.y = 5)

data_by_weeks %>%
  select(-q4) %>%
  replace(is.na(.), 0) %>%
  mutate(week_private = mo*10^5+tu*10^4+we*10^3+th*10^2+fr*10+sa) %>%
  group_by(week_private) %>%
  dplyr::summarize(mos = mean(q2), f_mean = mean(f_mean), n = n()) %>%
  ggscatter(x = "f_mean", y = "mos",
            add = "reg.line",  # Add regressin line
            add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE # Add confidence interval
  ) + 
  stat_cor(method = "pearson", label.x = 2.5, label.y = 5)

data_by_weeks %>%
  select(-q4) %>%
  replace(is.na(.), 0) %>%
  mutate(week_private = mo*10^5+tu*10^4+we*10^3+th*10^2+fr*10+sa) %>%
  group_by(week_private) %>%
  dplyr::summarize(mos = mean(q2), f_lin_last = mean(f_lin_last), n = n()) %>%
  ggscatter(x = "f_lin_last", y = "mos",
            add = "reg.line",  # Add regressin line
            add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE # Add confidence interval
  ) + 
  stat_cor(method = "pearson", label.x = 2.5, label.y = 5)

data_by_weeks %>%
  select(-q4) %>%
  replace(is.na(.), 0) %>%
  mutate(week_private = mo*10^5+tu*10^4+we*10^3+th*10^2+fr*10+sa) %>%
  group_by(week_private) %>%
  dplyr::summarize(mos = mean(q2), f_lin_first = mean(f_lin_first), n = n()) %>%
  ggscatter(x = "f_lin_first", y = "mos",
            add = "reg.line",  # Add regressin line
            add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE # Add confidence interval
  ) + 
  stat_cor(method = "pearson", label.x = 2.5, label.y = 5)

data_by_weeks %>%
  select(-q4) %>%
  replace(is.na(.), 0) %>%
  mutate(week_private = mo*10^5+tu*10^4+we*10^3+th*10^2+fr*10+sa) %>%
  group_by(week_private) %>%
  dplyr::summarize(mos = mean(q2), f_lin_ends = mean(f_lin_ends), n = n()) %>%
  ggscatter(x = "f_lin_ends", y = "mos",
            add = "reg.line",  # Add regressin line
            add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE # Add confidence interval
  ) + 
  stat_cor(method = "pearson", label.x = 2.5, label.y = 5)

#and taking into account number of votes


data_by_weeks %>%
  select(-q4) %>%
  replace(is.na(.), 0) %>%
  mutate(week_private = mo*10^5+tu*10^4+we*10^3+th*10^2+fr*10+sa) %>%
  group_by(week_private) %>%
  dplyr::summarize(mos = mean(q2), f_mean = mean(f_mean), n = n(), week_private = week_private) %>%
  ggscatter(x = "f_mean", y = "mos",
            add = "reg.line",  # Add regressin line
            add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE # Add confidence interval
  ) + 
  stat_cor(method = "pearson", label.x = 2.5, label.y = 5)

data_by_weeks %>%
  select(-q4) %>%
  replace(is.na(.), 0) %>%
  mutate(week_private = mo*10^5+tu*10^4+we*10^3+th*10^2+fr*10+sa) %>%
  group_by(week_private) %>%
  dplyr::summarize(mos = mean(q2), f_lin_last = mean(f_lin_last), n = n(), week_private = week_private) %>%
  ggscatter(x = "f_lin_last", y = "mos",
            add = "reg.line",  # Add regressin line
            add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE # Add confidence interval
  ) + 
  stat_cor(method = "pearson", label.x = 2.5, label.y = 5)

data_by_weeks %>%
  select(-q4) %>%
  replace(is.na(.), 0) %>%
  mutate(week_private = mo*10^5+tu*10^4+we*10^3+th*10^2+fr*10+sa) %>%
  group_by(week_private) %>%
  dplyr::summarize(mos = mean(q2), f_lin_first = mean(f_lin_first), n = n(), week_private = week_private) %>%
  ggscatter(x = "f_lin_first", y = "mos",
            add = "reg.line",  # Add regressin line
            add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE # Add confidence interval
  ) + 
  stat_cor(method = "pearson", label.x = 2.5, label.y = 5)

data_by_weeks %>%
  select(-q4) %>%
  replace(is.na(.), 0) %>%
  mutate(week_private = mo*10^5+tu*10^4+we*10^3+th*10^2+fr*10+sa) %>%
  group_by(week_private) %>%
  dplyr::summarize(mos = mean(q2), f_lin_ends = mean(f_lin_ends), n = n(), week_private = week_private) %>%
  ggscatter(x = "f_lin_ends", y = "mos",
            add = "reg.line",  # Add regressin line
            add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE # Add confidence interval
  ) + 
  stat_cor(method = "pearson", label.x = 2.5, label.y = 5)



data_by_weeks$os <- factor(data_by_weeks$q2, ordered = TRUE, levels = c(1, 2, 3, 4, 5))

model_mean <- clm(os ~ f_mean, data = data_by_weeks, family = "binomial")
summary(model_mean)

model_lin_last <- clm(os ~ f_lin_last, data = data_by_weeks, family = "binomial")
summary(model_lin_last)

model_lin_first <- clm(os ~ f_lin_first, data = data_by_weeks, family = "binomial")
summary(model_lin_first)

model_lin_ends <- clm(os ~ f_lin_ends, data = data_by_weeks, family = "binomial")
summary(model_lin_ends)

model_lin_ends <- clm(os ~ f_mean + f_lin_last + f_lin_first + f_lin_ends, data = data_by_weeks, family = "binomial")
summary(model_lin_ends)

data_only_full <- data_by_weeks[data_by_weeks$n == 7,]

model_mean <- clm(os ~ f_mean, data = data_only_full, family = "binomial")
summary(model_mean)

model_lin_last <- clm(os ~ f_lin_last, data = data_only_full, family = "binomial")
summary(model_lin_last)

model_lin_first <- clm(os ~ f_lin_first, data = data_only_full, family = "binomial")
summary(model_lin_first)

model_lin_ends <- clm(os ~ f_lin_ends, data = data_only_full, family = "binomial")
summary(model_lin_ends)

model_lin_ends <- clm(os ~ f_mean + f_lin_last + f_lin_first + f_lin_ends, data = data_only_full, family = "binomial")
summary(model_lin_ends)

data_by_weeks <- data_by_weeks_1204

data_by_weeks$os <- factor(data_by_weeks$q2, ordered = TRUE, levels = c(1, 2, 3, 4, 5))


# steps <- c(0.1428571, 0.2857143, 0.4285714, 0.5714286, 0.7142857, 0.8571429)
steps <- c(0.01, 0.2, 0.4, 0.6, 0.8, 0.99)

f_param <- function(x, a, b) {
  norm_vec <- t(replicate(nrow(x), kumaraswa_den(steps, a, b)))
  norm = norm_vec[,1]
  norm[is.na(x[,1])] <- 0
  tmp <- x[,1]
  tmp[is.na(x[,1])] <- 0
  tmp <- tmp * norm
  for (i in c(2:6)){
    tmp_norm <- norm_vec[,i]
    tmp_norm[is.na(x[,i])] <- 0
    norm <- norm + tmp_norm
    tmp_na <- x[,i]
    tmp_na[is.na(tmp_na)] <- 0
    tmp <- tmp + tmp_na * tmp_norm
  }
  return(tmp / norm)
}

objective_fun <- function(params, df) {
  a <- params[1]
  b <- params[2]
  
  df$f_param <- f_param(as.matrix(df[,c("mo", "tu", "we", "th", "fr", "sa")]), a, b)
  
  model <- clm(os ~ f_param, data = df, family = "binomial")  
  
  return(AIC(model))  
}
init_params <- c(1.0, 1.0)
lower_bounds <- c(0.00001, 0.00001)
opt_data <- data_by_weeks %>%
  filter(n < 8)

opt_result <- optimx(init_params, objective_fun, method="L-BFGS-B", 
                     lower=lower_bounds, df = opt_data)

best_params <- opt_result
print(best_params)

plot(steps, kumaraswa_den(steps, best_params$p1, best_params$p2)/sum(kumaraswa_den(steps, best_params$p1, best_params$p2)))

data_by_weeks$f_param <- f_param(as.matrix(
  data_by_weeks[,c("mo", "tu", "we", "th", "fr", "sa")]), 
  best_params$p1, best_params$p2)

data_by_weeks %>%
  select(-q4) %>%
  replace(is.na(.), 0) %>%
  mutate(week_private = mo*10^5+tu*10^4+we*10^3+th*10^2+fr*10+sa) %>%
  group_by(week_private) %>%
  dplyr::summarize(mos = mean(q2), f_param = mean(f_param), n = n(), week_private = week_private) %>%
  ggscatter(x = "f_param", y = "mos",
            add = "reg.line",  # Add regression line
            add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE # Add confidence interval
  ) + 
  stat_cor(method = "pearson", label.x = 2.5, label.y = 5)

#comparing real MOS with modelled MOS for Kumaraswa

d_mos_k <- data_by_weeks %>%
  group_by(week) %>% 
  dplyr::summarize(n_votes = n(), mos_real = mean(q2, na.rm = TRUE), mos_k = mean(f_param, na.rm = TRUE), sd_real = sd(q2, na.rm = TRUE), sd_k = sd(f_param, na.rm = TRUE))

#and comparison when we only have full weeks

d_mos_full_k <- data_by_weeks %>% filter(n == 7)  %>% 
  group_by(week) %>% 
  dplyr::summarize(n_votes = n(), mos_real = mean(q2, na.rm = TRUE), mos_k = mean(f_param, na.rm = TRUE), sd_real = sd(q2, na.rm = TRUE), sd_k = sd(f_param, na.rm = TRUE))

d_mos_full_k$diff <- d_mos_full_k$mos_real - d_mos_full_k$mos_k

d_mos_full_k %>%
  ggplot(aes(week, mos_real)) + geom_errorbar(aes(ymin = mos_real - 1.96*sd_real/sqrt(n_votes), ymax = mos_real + 1.96*sd_real/sqrt(n_votes))) + geom_point(aes(week, mos_k), color = 'blue')

model <- clm(os ~ mo  + we + sa, data = data_by_weeks)
summary(model)

data_all_in <- data_by_weeks %>%
  filter(n == 7) %>%
  mutate(tu = tu + runif(n = 311, min = -0.3, max = 0.3)) %>%
  mutate(th = th + runif(n = 311, min = -0.3, max = 0.3)) %>%
  mutate(sa = sa + runif(n = 311, min = -0.3, max = 0.3)) 

model <- clm(os ~ mo + tu + we + th + fr + sa, data = data_all_in)
summary(model)

cor(data_all_in$mo, data_all_in$tu)

model_3var <- clm(os ~ tu + th + sa, data = data_all_in)
summary(model_3var)
AIC(model_3var)

data_all_in$test_var <- data_all_in$tu*0.506 + data_all_in$th*0.789 + data_all_in$sa*0.810
model_1var <- clm(os ~ test_var, data = data_all_in)
summary(model_1var)
AIC(model_1var)


data_all_in$test_var <- data_all_in$tu*1.506 + data_all_in$th*0.289 + data_all_in$sa*0.310
model_1var <- clm(os ~ test_var, data = data_all_in)
summary(model_1var)
AIC(model_1var)
