library(jsonlite)
library(tidyverse)
library(purrr)
library(ggplot2)
library(plyr)
library(gtools)
library(tidyr)
library(tidyverse)
library(reshape2)


summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


temp = list.files(pattern="*.json")

for (var in 1:38) {
  
  json_data <- read_json(path=temp[var])
  l = length(json_data)
  my_list <-json_data[1]
  my_list2 <- unlist(my_list,recursive=F)               
  new_list <- my_list2[1][c("external_id")]
  my_list3 <- unlist(my_list2,recursive=F) 
  df_s <- c()
  df_s$external_id <- my_list3[["external_id"]]
  df_s$video <- my_list3[["result.video"]]
  df_s$z <- my_list3[["result.accelerometer_data"]][["z"]]
  if(length(df_s$z) == 0){
    df_s$z[1] = NA
  }
  new_dataframe <- df_s$z
  my_df_z <- do.call(rbind.data.frame, new_dataframe)
  my_df_z$user <- 1
  colnames(my_df_z) <- c("z", "user")
  df_mean <- summarySE(my_df_z, measurevar="z", groupvars=c("user"), na.rm = TRUE)
  df_s <- data.frame(df_s$external_id, df_s$video, df_mean$z, df_mean$sd)
  colnames(df_s) <- c('external_id','video', 'mean_z', 'z_sd')
  df_s <- df_s[!grepl("codec", df_s$video),]
  df_s <- df_s[!grepl("src", df_s$video),]
  for (x in 2: l) {
    my_list <-json_data[x]
    my_list2 <- unlist(my_list,recursive=F)               
    new_list <- my_list2[1][c("external_id")]
    my_list3 <- unlist(my_list2,recursive=F) 
    dfz <- c()
    dfz$external_id <- my_list3[["external_id"]]
    dfz$video <- my_list3[["result.video"]]
    dfz$z <- my_list3[["result.accelerometer_data"]][["z"]]
    if(length(dfz$z) == 0){
      dfz$z[1] = NA
    }
    new_dataframe <- dfz$z
    my_df_z <- do.call(rbind.data.frame, new_dataframe)
    my_df_z$user <- 1
    colnames(my_df_z) <- c("z", "user")
    df_mean <- summarySE(my_df_z, measurevar="z", groupvars=c("user"), na.rm = TRUE)
    dfz <- data.frame(dfz$external_id, dfz$video, df_mean$z, df_mean$sd)
    colnames(dfz) <- c('external_id','video', 'mean_z', 'z_sd')
    dfz <- dfz[!grepl("codec", dfz$video),]
    dfz <- dfz[!grepl("src", dfz$video),]
    df_s <- rbind(df_s, dfz)
    
  }
  a <- substr(df_s$external_id[1], 1, 7)
  name <- sprintf("accelo_%s.csv", a)
  write.csv(df_s, name, row.names = FALSE)
}

