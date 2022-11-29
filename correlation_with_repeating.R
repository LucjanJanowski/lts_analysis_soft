library(tidyverse)

data_dif <- read_csv("dataframe_MOS_difference.csv")
data_dif <- data_dif %>%
  mutate(vmaf = as.numeric(str_extract(video, ".."))) %>%
  mutate(user = str_extract(external_id, "...."))

res <- data_dif %>%
  group_by(external_id) %>%
  summarize(cor = cor(q2, q2_repeat), n = n())
res %>%
  ggplot(aes(external_id, cor, color = n)) + geom_point(size = 4)

data_dif %>%
  filter(external_id == "343ab3af9c806c97a240534e27177c3230347c333dd171df36f0593ac53c2bef") %>%
  ggplot(aes(q2, q2_repeat)) + 
  geom_count() +
  labs(title = "343ab3af9c806")


tmp <- data_dif %>%
  filter(external_id == "343ab3af9c806c97a240534e27177c3230347c333dd171df36f0593ac53c2bef")

data_dif %>%
  filter(external_id == "898c19efb1ec4f9ab886b4a4e4abff904c6c3b1fdde0d70cfbd84214ed12e8dc") %>%
  ggplot(aes(q2, q2_repeat)) + geom_count()

users <- unique(data_dif$user)
for (user_idx in users){
  p <- data_dif %>%
    filter(user == user_idx) %>%
    ggplot(aes(vmaf, q2)) + 
      geom_point(color = "green") + 
      geom_jitter(aes(vmaf, q2_repeat), color = "red", 
                  position = position_jitter(h=0.1, w=0)) + 
      labs(title = sprintf("cor = %.3f for user = %s", 
                           cor(data_dif$q2[data_dif$user == user_idx], 
                               data_dif$q2_repeat[data_dif$user == user_idx]), 
                           user_idx))
  print(p)
}

 