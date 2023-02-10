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
if ("q2" %in% my_lis7 == TRUE) {
  new_list <- my_lis7[c("external_id", "date", "result.video", "result.reply_form.content.answer.value")]
  new_list$result.reply_form.content.answer.value[is_null(new_list$result.reply_form.content.answer.value)] = NA
  my_df <- do.call(rbind.data.frame, new_list)
  df_s2<-as.data.frame(t(my_df))
  colnames(df_s2) <- c('external_id', 'date', 'video', 'q2_2')
}else {
  print("oops")
  new_list0 <- my_lis5[c("external_id", "date", "result.video")]
  my_df0 <- do.call(rbind.data.frame, new_list0)
  df_s2<-as.data.frame(t(my_df0))
  colnames(df_s2) <- c('external_id', 'date', 'video')
  q2_2 <- c(NA)
  df_s2$q2_2 <- q2_2
}
for (x in 2: l) {
  my_list <-json_data[x]
  my_lis2 <- unlist(my_list,recursive=F)               
  my_lis3 <- unlist(my_lis2,recursive=F)  
  my_lis4 <- unlist(my_lis3,recursive=F)  
  my_lis5 <- unlist(my_lis4,recursive=F) 
  my_lis6 <- unlist(my_lis5,recursive=F)
  my_lis7 <- unlist(my_lis6,recursive=F) 
  if ("q2" %in% my_lis7 == TRUE) {
    new_list <- my_lis7[c("external_id", "date", "result.video", "result.reply_form.content.answer.value")]
    new_list$result.reply_form.content.answer.value[is_null(new_list$result.reply_form.content.answer.value)] = NA
    my_df <- do.call(rbind.data.frame, new_list)
    dfx<-as.data.frame(t(my_df))
    colnames(dfx) <- c('external_id', 'date', 'video', 'q2_2')
    df_s2 <- rbind(df_s2, dfx)
  }
  else {
    print("oops")
    new_list0 <- my_lis5[c("external_id", "date", "result.video")]
    my_df0 <- do.call(rbind.data.frame, new_list0)
    dfz<-as.data.frame(t(my_df0))
    colnames(dfz) <- c('external_id', 'date', 'video')
    q2_2 <- c(NA)
    dfz$q2_2 <- q2_2
    df_s2 <- rbind(df_s2, dfz)
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
    if ("q2" %in% my_lis7 == TRUE) {
      new_list <- my_lis7[c("external_id", "date", "result.video", "result.reply_form.content.answer.value")]
      new_list$result.reply_form.content.answer.value[is_null(new_list$result.reply_form.content.answer.value)] = NA
      my_df <- do.call(rbind.data.frame, new_list)
      dfx<-as.data.frame(t(my_df))
      colnames(dfx) <- c('external_id', 'date', 'video', 'q2_2')
      df_s2 <- rbind(df_s2, dfx)
    }
    else {
      print("oops")
      new_list0 <- my_lis5[c("external_id", "date", "result.video")]
      my_df0 <- do.call(rbind.data.frame, new_list0)
      dfz<-as.data.frame(t(my_df0))
      colnames(dfz) <- c('external_id', 'date', 'video')
      q2_2 <- c(NA)
      dfz$q2_2 <- q2_2
      df_s2 <- rbind (df_s2, dfz)
    }
    
  }
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
if ("q3" %in% my_lis7 == TRUE) {
  new_list <- my_lis7[c("external_id", "date", "result.video", "result.reply_form.content2.answer.value")]
  new_list$result.reply_form.content2.answer.value[is_null(new_list$result.reply_form.content2.answer.value)] = NA
  my_df <- do.call(rbind.data.frame, new_list)
  df_s<-as.data.frame(t(my_df))
  colnames(df_s) <- c('external_id', 'date', 'video', 'q2_2')
}else {
  print("oops")
  new_list0 <- my_lis5[c("external_id", "date", "result.video")]
  my_df0 <- do.call(rbind.data.frame, new_list0)
  df_s<-as.data.frame(t(my_df0))
  colnames(df_s) <- c('external_id', 'date', 'video')
  q2_2 <- c(NA)
  df_s$q2_2 <- q2_2
}
for (x in 2: l) {
  my_list <-json_data[x]
  my_lis2 <- unlist(my_list,recursive=F)               
  my_lis3 <- unlist(my_lis2,recursive=F)  
  my_lis4 <- unlist(my_lis3,recursive=F)  
  my_lis5 <- unlist(my_lis4,recursive=F) 
  my_lis6 <- unlist(my_lis5,recursive=F)
  my_lis7 <- unlist(my_lis6,recursive=F) 
  if ("q3" %in% my_lis7 == TRUE) {
    new_list <- my_lis7[c("external_id", "date", "result.video", "result.reply_form.content2.answer.value")]
    new_list$result.reply_form.content2.answer.value[is_null(new_list$result.reply_form.content2.answer.value)] = NA
    my_df <- do.call(rbind.data.frame, new_list)
    dfx<-as.data.frame(t(my_df))
    colnames(dfx) <- c('external_id', 'date', 'video', 'q2_2')
    df_s <- rbind(df_s, dfx)
  }
  else {
    print("oops")
    new_list0 <- my_lis5[c("external_id", "date", "result.video")]
    my_df0 <- do.call(rbind.data.frame, new_list0)
    dfz<-as.data.frame(t(my_df0))
    colnames(dfz) <- c('external_id', 'date', 'video')
    q2_2 <- c(NA)
    dfz$q2_2 <- q2_2
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
    if ("q3" %in% my_lis7 == TRUE) {
      new_list <- my_lis7[c("external_id", "date", "result.video", "result.reply_form.content2.answer.value")]
      new_list$result.reply_form.content2.answer.value[is_null(new_list$result.reply_form.content2.answer.value)] = NA
      my_df <- do.call(rbind.data.frame, new_list)
      dfx<-as.data.frame(t(my_df))
      colnames(dfx) <- c('external_id', 'date', 'video', 'q2_2')
      df_s <- rbind(df_s, dfx)
    }
    else {
      print("oops")
      new_list0 <- my_lis5[c("external_id", "date", "result.video")]
      my_df0 <- do.call(rbind.data.frame, new_list0)
      dfz<-as.data.frame(t(my_df0))
      colnames(dfz) <- c('external_id', 'date', 'video')
      q2_2 <- c(NA)
      dfz$q2_2 <- q2_2
      df_s <- rbind (df_s, dfz)
    }
    
  }
}







df_sc2 <- df_s2[grepl("p1204", df_s2$video),]

df_sc <- df_s[grepl("p1204", df_s$video),]

df_sc$q2_1 <- as.numeric(df_sc2$q2_2)
df_sc$q2_2 <- as.numeric(df_sc$q2_2)
df_sc$q2 <- rowSums(df_sc[,c("q2_2", "q2_1")], na.rm=TRUE)
write.csv(df_sc,"phase4_calib_lts.csv", row.names = FALSE)
df_sc$q2 <- as.numeric(df_sc$q2)

moses_calib <- df_sc %>%
  group_by(video) %>%
  dplyr::summarize(mos = mean(q2, na.rm = TRUE), sd = sd(q2, na.rm = TRUE))

moses_calib$video[moses_calib$video == '01_p1204_2.0__ph_nature055.mp4'] <- '01_p1204_2.0_4ph_nature055.mp4'
moses_calib$video[moses_calib$video == '04_p1204_4.0__4ph_nature053.mp4'] <- '04_p1204_4.0_4ph_nature053.mp4'
moses_calib2 <- separate(moses_calib, video,  into = c("number", "metrics", "quality", "name"), sep = "_")
write.csv(moses_calib2,"phase4_calib_moses.csv", row.names = FALSE)
