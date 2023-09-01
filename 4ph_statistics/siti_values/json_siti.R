library(jsonlite)
library(tidyverse)
library(purrr)
library(ggplot2)
library(plyr)
library(gtools)
library(tidyr)
library(tidyverse)

temp = list.files(pattern="*.json")
si_ti <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(si_ti) <- c('file', 'si', 'ti', 'content_type')
var = length(temp)
for (y in 1: var) {
  json_data <- read_json(path=temp[y])
  my_list <- unlist(json_data, recursive = FALSE)
  l = length(json_data)
  si <- my_list[["avg_si"]]
  ti <- my_list[["avg_ti"]]
  if (grepl("widoki",temp[y])==TRUE){
  si_ti[nrow(si_ti) + 1,] <- c(temp[y], si, ti, "nature")}
  else if (grepl("slowmo",temp[y])==TRUE){
  si_ti[nrow(si_ti) + 1,] <- c(temp[y], si, ti, "slow motion")}  
}

write.csv(si_ti,"si_ti_4ph.csv", row.names = FALSE)

