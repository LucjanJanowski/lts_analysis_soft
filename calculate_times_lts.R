library(tidyverse)
library(ggpubr)
a <- read_csv("dataframe_times_phase2.csv", col_types = "f-fnnTT")
a$time_diff <- a$q2_stop - a$q2_start
a %>% group_by(external_id) %>% summarize(mos = mean(time_diff, na.rm = TRUE)) -> b
#df_times <- summarySE(a, measurevar="time_diff", groupvars=c("external_id"), na.rm = TRUE)
