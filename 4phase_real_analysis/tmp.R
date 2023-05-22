f_lin_last <- function(x) {
  tmp <- x[,1]
  tmp[is.na(x[,1])] <- 0
  norm <- rep(1, nrow(x))
  norm[is.na(x[,1])] <- 0
  for (i in c(2:6)){
    tmp_norm <- rep(i, nrow(x))
    tmp_norm[is.na(x[,i])] <- 0
    norm <- norm + tmp_norm
    tmp_na <- x[,i]
    tmp_na[is.na(tmp_na)] <- 0
    tmp <- tmp + tmp_na * tmp_norm
  }
  return(tmp / norm)
}


data_by_weeks$f_lin_last = f_lin_last(as.matrix(data_by_weeks[,c("mo", "tu", "we", "th", "fr", "sa")]))

