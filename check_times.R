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
json_data <- read_json(path=temp[1])
l = length(json_data)
my_list <-json_data[1]
my_lis2 <- unlist(my_list,recursive=F)               
my_lis3 <- unlist(my_lis2,recursive=F)  
my_lis4 <- unlist(my_lis3,recursive=F)  
my_lis5 <- unlist(my_lis4,recursive=F)  
my_lis6 <- unlist(my_lis5,recursive=F)  
my_lis7 <- unlist(my_lis6,recursive=F)  
new_list <- my_lis7[c("external_id", "date", "result.video", "result.reply_form.content1.answer.value", "result.reply_form.content2.answer.value", "result.reply_form.content2.answer.started", "result.reply_form.content2.answer.timestamp", "result.device_info.brand")]
new_list$result.reply_form.content1.answer.value[is_null(new_list$result.reply_form.content1.answer.value)] = NA
new_list$result.reply_form.content2.answer.value[is_null(new_list$result.reply_form.content2.answer.value)] = NA
new_list$result.reply_form.content2.answer.started[is_null(new_list$result.reply_form.content2.answer.started)] = NA
new_list$result.reply_form.content2.answer.timestamp[is_null(new_list$result.reply_form.content2.answer.timestamp)] = NA
my_df <- do.call(rbind.data.frame, new_list)
df_s<-as.data.frame(t(my_df))
colnames(df_s) <- c('external_id', 'date', 'video', 'q1', 'q2', 'q2_start', 'q2_stop', 'brand')
for (x in 2: l) {
  my_list <-json_data[x]
  my_lis2 <- unlist(my_list,recursive=F)               
  my_lis3 <- unlist(my_lis2,recursive=F)  
  my_lis4 <- unlist(my_lis3,recursive=F)  
  my_lis5 <- unlist(my_lis4,recursive=F) 
  my_lis6 <- unlist(my_lis5,recursive=F)
  my_lis7 <- unlist(my_lis6,recursive=F) 
  if ("q1" %in% my_lis7 == TRUE) {
    new_list <- my_lis7[c("external_id", "date", "result.video", "result.reply_form.content1.answer.value", "result.reply_form.content2.answer.value", "result.reply_form.content2.answer.started", "result.reply_form.content2.answer.timestamp", "result.device_info.brand")]
    new_list$result.reply_form.content1.answer.value[is_null(new_list$result.reply_form.content1.answer.value)] = NA
    new_list$result.reply_form.content2.answer.value[is_null(new_list$result.reply_form.content2.answer.value)] = NA
    new_list$result.reply_form.content2.answer.started[is_null(new_list$result.reply_form.content2.answer.started)] = NA
    new_list$result.reply_form.content2.answer.timestamp[is_null(new_list$result.reply_form.content2.answer.timestamp)] = NA
    my_df <- do.call(rbind.data.frame, new_list)
    dfx<-as.data.frame(t(my_df))
    colnames(dfx) <- c('external_id', 'date', 'video', 'q1', 'q2', 'q2_start', 'q2_stop', 'brand')
    df_s <- rbind(df_s, dfx)
  }
  else {
    print("oops")
    new_list0 <- my_lis5[c("external_id", "date", "result.video")]
    my_df0 <- do.call(rbind.data.frame, new_list0)
    dfz<-as.data.frame(t(my_df0))
    colnames(dfz) <- c('external_id', 'date', 'video')
    q1 <- c(NA)
    q2 <- c(NA)
    q2_start <- c(NA)
    q2_stop <- c(NA)
    brand <- c(NA)
    dfz$q1 <- q1
    dfz$q2 <- q2
    dfz$q2_start <- q2_start
    dfz$q2_stop <- q2_stop
    dfz$brand <- brand
    df_s <- rbind(df_s, dfz)
  }
  
}
for (var in 2:38) {
  json_data <- read_json(path=temp[var])
  l = length(json_data)
  for (x in 1: l) {
    my_list <-json_data[x]
    my_lis2 <- unlist(my_list,recursive=F)               
    my_lis3 <- unlist(my_lis2,recursive=F)  
    my_lis4 <- unlist(my_lis3,recursive=F)  
    my_lis5 <- unlist(my_lis4,recursive=F) 
    my_lis6 <- unlist(my_lis5,recursive=F)
    my_lis7 <- unlist(my_lis6,recursive=F) 
    if ("q1" %in% my_lis7 == TRUE) {
      new_list <- my_lis7[c("external_id", "date", "result.video", "result.reply_form.content1.answer.value", "result.reply_form.content2.answer.value", "result.reply_form.content2.answer.started", "result.reply_form.content2.answer.timestamp", "result.device_info.brand")]
      new_list$result.reply_form.content1.answer.value[is_null(new_list$result.reply_form.content1.answer.value)] = NA
      new_list$result.reply_form.content2.answer.value[is_null(new_list$result.reply_form.content2.answer.value)] = NA
      new_list$result.reply_form.content2.answer.started[is_null(new_list$result.reply_form.content2.answer.started)] = NA
      new_list$result.reply_form.content2.answer.timestamp[is_null(new_list$result.reply_form.content2.answer.timestamp)] = NA
      my_df <- do.call(rbind.data.frame, new_list)
      dfx<-as.data.frame(t(my_df))
      colnames(dfx) <- c('external_id', 'date', 'video', 'q1', 'q2', 'q2_start', 'q2_stop', 'brand')
      df_s <- rbind(df_s, dfx)
    }
    else {
      print("oops")
      new_list0 <- my_lis5[c("external_id", "date", "result.video")]
      my_df0 <- do.call(rbind.data.frame, new_list0)
      dfz<-as.data.frame(t(my_df0))
      colnames(dfz) <- c('external_id', 'date', 'video')
      q1 <- c(NA)
      q2 <- c(NA)
      q2_start <- c(NA)
      q2_stop <- c(NA)
      brand <- c(NA)
      dfz$q1 <- q1
      dfz$q2 <- q2
      dfz$q2_start <- q2_start
      dfz$q2_stop <- q2_stop
      dfz$brand <- brand
      df_s <- rbind (df_s, dfz)
    }
    
  }
}

df_s <- df_s[!grepl("codec", df_s$video),]
df_s <- df_s[!grepl("src", df_s$video),]
df_times <- summarySE(df_s, measurevar="q2", groupvars=c("external_id"), na.rm = TRUE)
df_try <- df_s[ !duplicated(df_s$external_id), ]
write.csv(df_s,"dataframe_times_phase2.csv", row.names = FALSE)
