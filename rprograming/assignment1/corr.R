corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  files <- list.files(directory, pattern="*.csv", full.names=T)
  data <- lapply(files, function(x) read.csv(x, header=T))
  df <- do.call(rbind.data.frame, data)
  
  df.cor <- c()
  
  for (i in 1:length(files)) {
    df.for <- df[df$ID == i,]
    s <- sum(complete.cases(df.for))
    if (s > threshold) {
      co <- cor(x=df.for$sulfate,y=df.for$nitrate, use="complete.obs")
      df.cor <- c(df.cor, co)
    }
  }
  
  df.cor
}