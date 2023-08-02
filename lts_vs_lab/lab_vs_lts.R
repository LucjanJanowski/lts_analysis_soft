library(tidyverse)
library(dplyr)
library(ggplot2)
library(dplyr)
lab <- read_csv("phase4_lab.csv", col_types = "fnnc")
lts_all <- read_csv("phase4_lts.csv", col_types = "fnnnnnnnnnnnncc")
lts <- lts_all %>% filter(n == 7)
lts$group <- "lts"
lts <- subset(lts, select = c(external_id, week, q2, group, week_q))

d_mos_lab <- lab %>% 
  group_by(week) %>% 
  dplyr::summarize(n_votes = n(), mos = mean(q2, na.rm = TRUE), sd = sd(q2, na.rm = TRUE), group = "lab")

d_mos_lts <- lts %>% 
  group_by(week) %>% 
  dplyr::summarize(n_votes = n(), mos = mean(q2, na.rm = TRUE), sd = sd(q2, na.rm = TRUE), group = "lts")

mos_compare <- rbind(d_mos_lab, d_mos_lts)

mos_compare %>%
  ggplot(aes(week, mos, color = group)) + geom_point() + geom_errorbar(aes(ymin = mos - 1.96*sd/sqrt(n_votes), ymax = mos + 1.96*sd/sqrt(n_votes)))

mos_compare <- mos_compare %>%
  add_column(v = NA)

mos_compare$v[mos_compare$week == 1] <- "5,5,2,2,5,5"
mos_compare$v[mos_compare$week == 2] <- "5,5,2,2,5,5"
mos_compare$v[mos_compare$week == 3] <- "5,5,5,5,2,2"
mos_compare$v[mos_compare$week == 4] <- "2,2,5,5,5,5"
mos_compare$v[mos_compare$week == 5] <- "4,4,2,2,4,4"
mos_compare$v[mos_compare$week == 6] <- "4,4,4,4,2,2"
mos_compare$v[mos_compare$week == 7] <- "2,2,4,4,4,4"
mos_compare$v[mos_compare$week == 8] <- "2,2,5,5,2,2"
mos_compare$v[mos_compare$week == 9] <- "2,2,2,2,5,5"
mos_compare$v[mos_compare$week == 10] <- "5,5,2,2,2,2"
mos_compare$v[mos_compare$week == 11] <- "2,2,4,4,2,2"
mos_compare$v[mos_compare$week == 12] <- "2,2,2,2,4,4"
mos_compare$v[mos_compare$week == 13] <- "4,4,2,2,2,2"

  
mos_compare$v <- data.frame(do.call("rbind", strsplit(as.character(mos_compare$v), ",", fixed = TRUE)))
mos_summary <- data.frame(mos_compare)
colnames(mos_summary) <- c("week", "n_votes", "mos", "sd", "group", "v1", "v2", "v3", "v4", "v5", "v6")

#save as csv
  write.csv(mos_summary,"mos_summary.csv", row.names = FALSE)

  