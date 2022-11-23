# Preliminary analysis for detecting irrelevant testers
# Created by Lucjan Janowski 07.10.2022


library(tidyverse)

scores <- read_csv("dataframe_lts_phase2.csv", col_type = "f-fnn")
summary(scores)

moses <- scores %>%
  group_by(video) %>%
  summarize(mos = mean(q2, na.rm = TRUE))

with_mos <- left_join(scores, moses)

res_cor <- with_mos %>%
  group_by(external_id) %>%
  summarize(cor_in = cor(q2, mos, use = "complete.obs")) 
res_cor %>%
  ggplot(aes(external_id, cor_in)) + geom_point()
with_mos_2 <- with_mos




for (i in 1:5){
  to_remove <- res_cor$external_id[res_cor$cor_in == min(res_cor$cor_in)]
  print(to_remove)
  print(min(res_cor$cor_in))
  with_mos_2 <- with_mos_2[with_mos_2$external_id != to_remove,]
  res_cor <- with_mos_2 %>%
    group_by(external_id) %>%
    summarize(cor_in = cor(q2, mos, use = "complete.obs")) 
  res_cor %>%
    ggplot(aes(external_id, cor_in)) + geom_point() -> p
  print(p)
}

scores %>%
  mutate(score = fct_rev(as.factor(q2))) %>%
  ggplot(aes(x = external_id, fill = score)) + 
    geom_bar(position = "stack")

{
scores %>%
  mutate(score = fct_rev(as.factor(q2))) -> a
  b <- a %>%
    group_by(external_id) %>%
    summarize(s1 = 100*sum(score == 1, na.rm = TRUE) + sum(score == 2, na.rm = TRUE))
  c <- left_join(a, b)
  ggplot(c[order(c$s1), ], aes(x = external_id, y = s1, fill = score)) + 
    geom_bar(position = "stack", stat = "identity")
}

