library(tidyverse)
library(ggpubr)
a <- read_csv("dataframe_lts_phase2.csv", col_types = "f-fnn")
summary(a)

a %>% group_by(video) %>% summarize(mos = mean(q2, na.rm = TRUE)) -> b

dataframe <- a %>% group_split(external_id)

for (x in 1: 38) {
  df1 <- do.call(rbind.data.frame, dataframe[x])
  print(cor.test(df1$q1, df1$q2, method="pearson"))
}
#df_mos <- summarySE(df_s, measurevar="q2", groupvars=c("video"), na.rm = TRUE)
#df_with_mos <- left_join(a, b)
