library(tidyverse)

data_dif <- read_csv("dataframe_MOS_difference.csv")
res <- data_dif %>%
  group_by(external_id) %>%
  summarize(cor = cor(q2, q2_repeat))

