library(tidyverse)
library(dplyr)
library(ggplot2)
scores_ph4 <- read_csv("phase4_current_lts.csv", col_types = "fTfnlf")
scores_ph4$video_n <- substr(scores_ph4$video, 1, 3)
scores_ph4$video_n <- as.numeric(scores_ph4$video_n)
#df1 - bad quality in the middle
df1 <- scores_ph4 %>% filter(video_n < 8)
df1$metric <- as.numeric(substr(df1$video, 9, 10))
t20 <- df1 %>% filter(metric < 30)
t20_1 <- t20 %>%
  group_by(external_id) %>%
  dplyr::summarize(n_lowq = n()) 
dfs1 <- df1 %>%
  group_by(external_id) %>%
  dplyr::summarize(n = n(), q2 = q2, video_n = video_n) 
dfs1 <- dfs1[complete.cases(dfs1$q2),]
d1 <- left_join(dfs1, t20_1)

#df2 - bad quality in the middle
df2 <- scores_ph4 %>% filter(video_n < 15) %>% filter(video_n > 7) 
df2$metric <- as.numeric(substr(df2$video, 9, 10))
t20 <- df2 %>% filter(metric < 30)
t20_2 <- t20 %>%
  group_by(external_id) %>%
  dplyr::summarize(n_lowq = n())
dfs2 <- df2 %>%
  group_by(external_id) %>%
  dplyr::summarize(n = n(), q2 = q2, video_n = video_n) 
dfs2 <- dfs2[complete.cases(dfs2$q2),]
d2 <- left_join(dfs2, t20_2)

#df3 - bad quality in the middle
df3 <- scores_ph4 %>% filter(video_n < 22) %>% filter(video_n > 15) 
df3$metric <- as.numeric(substr(df3$video, 9, 10))
t20 <- df3 %>% filter(metric < 30)
t20_3 <- t20 %>%
  group_by(external_id) %>%
  dplyr::summarize(n_lowq = n())
dfs3 <- df3 %>%
  group_by(external_id) %>%
  dplyr::summarize(n = n(), q2 = q2, video_n = video_n) 
dfs3 <- dfs3[complete.cases(dfs3$q2),]
d3 <- left_join(dfs3, t20_3)

#df4 - bad quality in the middle
df4 <- scores_ph4 %>% filter(video_n < 29) %>% filter(video_n > 22)
df4$metric <- as.numeric(substr(df4$video, 9, 10))
t20 <- df4 %>% filter(metric < 30)
t20_4 <- t20 %>%
  group_by(external_id) %>%
  dplyr::summarize(n_lowq = n())
dfs4 <- df4 %>%
  group_by(external_id) %>%
  dplyr::summarize(n = n(), q2 = q2, video_n = video_n) 
dfs4 <- dfs4[complete.cases(dfs4$q2),]
d4 <- left_join(dfs4, t20_4)

#df5 - bad quality at the beginning
df5 <- scores_ph4 %>% filter(video_n < 36) %>% filter(video_n > 29)
df5$metric <- as.numeric(substr(df5$video, 9, 10))
t20 <- df5 %>% filter(metric < 30)
t20_5 <- t20 %>%
  group_by(external_id) %>%
  dplyr::summarize(n_lowq = n())
dfs5 <- df5 %>%
  group_by(external_id) %>%
  dplyr::summarize(n = n(), q2 = q2, video_n = video_n) 
dfs5 <- dfs5[complete.cases(dfs5$q2),]
d5 <- left_join(dfs5, t20_5)

#df6 - bad quality at the end
df6 <- scores_ph4 %>% filter(video_n < 43) %>% filter(video_n > 36) 
df6$metric <- as.numeric(substr(df6$video, 9, 10))
t20 <- df6 %>% filter(metric < 30)
t20_6 <- t20 %>%
  group_by(external_id) %>%
  dplyr::summarize(n_lowq = n())
dfs6 <- df6 %>%
  group_by(external_id) %>%
  dplyr::summarize(n = n(), q2 = q2, video_n = video_n) 
dfs6 <- dfs6[complete.cases(dfs6$q2),]
d6 <- left_join(dfs6, t20_6)
#number of missed videos
df_all <- rbind(d1, d2, d3, d4, d5, d6)
df_4weeks <- rbind(d1, d2, d3, d4)
nrow(df_4weeks[df_4weeks$n == '7', ])
print(cor.test(df_all$n, df_all$q2, method="pearson"))
print(cor.test(df_all$n_lowq, df_all$q2, method="pearson"))

df_ratings <- scores_ph4[complete.cases(scores_ph4$q2),]
#mos per week
moses_general <- df_ratings %>%
  group_by(video) %>%
  dplyr::summarize(mos = mean(q2, na.rm = TRUE), sd = sd(q2, na.rm = TRUE))
#correlation between testers and plots for each tester
df_mos_cor <- left_join(df_ratings, moses_general)
res_cor <- df_mos_cor %>%
  group_by(external_id) %>%
  dplyr::summarize(cor_in = cor(q2, mos, use = "complete.obs")) 


df_ratings$external_id <- substr(df_ratings$external_id, 1, 5) 
x <-split(df_ratings, df_ratings$external_id)
for (var in 1:38) {
  df <- data.frame(x[var])
  colnames(df) <- c('external_id', 'date', 'video', 'q2', 'q3', 'q4', 'video_n')
  a <- substr(df$external_id[1], 1, 5)
  name <- sprintf("ph4_per_tester_%s.csv", a)
  write.csv(df, name, row.names = FALSE)
  name_jpg <- sprintf("ph4_per_tester_%s.jpeg", a)
  jpeg(file= name_jpg)
  print(ggplot(df, aes(x=video_n, y=q2)) + geom_point() + ylim(1, 5))
  dev.off()
}

