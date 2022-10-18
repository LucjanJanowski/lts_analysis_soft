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
new_list <- my_lis7[c("external_id", "date", "result.video", "result.reply_form.content1.answer.value", "result.reply_form.content2.answer.value")]
new_list$result.reply_form.content1.answer.value[is_null(new_list$result.reply_form.content1.answer.value)] = NA
new_list$result.reply_form.content2.answer.value[is_null(new_list$result.reply_form.content2.answer.value)] = NA
my_df <- do.call(rbind.data.frame, new_list)
df_s<-as.data.frame(t(my_df))
colnames(df_s) <- c('external_id', 'date', 'video', 'q1', 'q2')
for (x in 2: l) {
  my_list <-json_data[x]
  my_lis2 <- unlist(my_list,recursive=F)               
  my_lis3 <- unlist(my_lis2,recursive=F)  
  my_lis4 <- unlist(my_lis3,recursive=F)  
  my_lis5 <- unlist(my_lis4,recursive=F) 
  my_lis6 <- unlist(my_lis5,recursive=F)
  my_lis7 <- unlist(my_lis6,recursive=F) 
  if ("q1" %in% my_lis7 == TRUE) {
    new_list <- my_lis7[c("external_id", "date", "result.video", "result.reply_form.content1.answer.value", "result.reply_form.content2.answer.value")]
    new_list$result.reply_form.content1.answer.value[is_null(new_list$result.reply_form.content1.answer.value)] = NA
    new_list$result.reply_form.content2.answer.value[is_null(new_list$result.reply_form.content2.answer.value)] = NA
    my_df <- do.call(rbind.data.frame, new_list)
    dfx<-as.data.frame(t(my_df))
    colnames(dfx) <- c('external_id', 'date', 'video', 'q1', 'q2')
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
    dfz$q1 <- q1
    dfz$q2 <- q2
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
  new_list <- my_lis7[c("external_id", "date", "result.video", "result.reply_form.content1.answer.value", "result.reply_form.content2.answer.value")]
  new_list$result.reply_form.content1.answer.value[is_null(new_list$result.reply_form.content1.answer.value)] = NA
  new_list$result.reply_form.content2.answer.value[is_null(new_list$result.reply_form.content2.answer.value)] = NA
  my_df <- do.call(rbind.data.frame, new_list)
  dfx<-as.data.frame(t(my_df))
  colnames(dfx) <- c('external_id', 'date', 'video', 'q1', 'q2')
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
  dfz$q1 <- q1
  dfz$q2 <- q2
  df_s <- rbind (df_s, dfz)
}

}
}

df_s <- df_s[!grepl("codec", df_s$video),]
df_s <- df_s[!grepl("src", df_s$video),]
write.csv(df_s,"dataframe_lts_phase2.csv", row.names = FALSE)
df_s <- separate(df_s, video,  into = c("number", "vmaf", "name"), sep = "_")
df_s$number <- as.numeric(df_s$number) - 30
df_forpy <- data.frame(df_s$external_id, df_s$number, df_s$q2)
df2_tp <- dcast(df_forpy, df_s.external_id ~ df_s.number)
df2_tp <- subset(df2_tp, select = -c(df_s.external_id) )
df2_matr <- data.matrix(df2_tp)
print(df2_tp)

write.csv(df2_tp, "~/matrix_2ndphase.csv")
df_s <- df_s[complete.cases(df_s$q2), ]
df_s$q2 <- as.numeric(df_s$q2)
df_ci <- data.frame(df_s$number, df_s$q2)
df_ci2 <- summarySE(df_ci, measurevar="df_s.q2", groupvars=c("df_s.number"))
#col <- c("d1_vmaf100", "d2_vmaf100", "d3_vmaf100", "d4_vmaf100", "d5_vmaf100", "d6_vmaf100", "d7_vmaf100", "d8_vmaf100", "d9_vmaf100", "d10_vmaf100", "d11_vmaf100", "d12_vmaf100", "d13_vmaf100", "d14_vmaf100", "d15_vmaf95", "d16_vmaf90", "d17_vmaf85", "d18_vmaf80", "d19_vmaf75", "d20_vmaf70", "d21_vmaf65", "d22_vmaf60", "d23_vmaf55", "d24_vmaf50", "d25_vmaf45", "d26_vmaf40", "d27_vmaf35", "d28_vmaf30", "d29_vmaf25", "d30_vmaf20")

col <- c("d1_vmaf100", "d2_vmaf100", "d3_vmaf100", "d4_vmaf100", "d5_vmaf100", "d6_vmaf100", "d7_vmaf100", "d8_vmaf100", "d9_vmaf100", "d10_vmaf100", "d11_vmaf100", "d12_vmaf100", "d13_vmaf100", "d14_vmaf100", "d15_vmaf96.0", "d16_vmaf90.7", "d17_vmaf85.8", "d18_vmaf81.6", "d19_vmaf76.2", "d20_vmaf72.2", "d21_vmaf67.3", "d22_vmaf62.0", "d23_vmaf59.2", "d24_vmaf51.8", "d25_vmaf46.4", "d26_vmaf44.1", "d27_vmaf36.5", "d28_vmaf36.4", "d29_vmaf27.8", "d30_vmaf24.0")

ggplot(df_ci2, aes(x=df_s.number, y=df_s.q2)) + 
  geom_errorbar(aes(ymin=df_s.q2-ci, ymax=df_s.q2+ci), width=.1) + geom_point() +  scale_x_continuous("day", labels = as.character(col), breaks = df_ci2$df_s.number) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + scale_y_continuous(name="MOS", limits=c(1, 5)) +  geom_vline(xintercept = 14.5, linetype="dotted", 
                                                                                                                                                                                                                                                                                                                            color = "blue", size=1.5)



mean <- aggregate(df_s$q2, list(df_s$number), FUN=mean) 
colnames(mean) <- c('day', 'MOS')
mean$day <- as.factor(mean$day)
col <- c("d1_vmaf100", "d2_vmaf100", "d3_vmaf100", "d4_vmaf100", "d5_vmaf100", "d6_vmaf100", "d7_vmaf100", "d8_vmaf100", "d9_vmaf100", "d10_vmaf100", "d11_vmaf100", "d12_vmaf100", "d13_vmaf100", "d14_vmaf100", "d15_vmaf95", "d16_vmaf90", "d17_vmaf85", "d18_vmaf80", "d19_vmaf75", "d20_vmaf70", "d21_vmaf65", "d22_vmaf60", "d23_vmaf55", "d24_vmaf50", "d25_vmaf45", "d26_vmaf40", "d27_vmaf35", "d28_vmaf30", "d29_vmaf25", "d30_vmaf20")
ggplot(mean, aes(day, MOS)) + geom_point() + scale_x_discrete(labels = col) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
