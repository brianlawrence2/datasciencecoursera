complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  files <- list.files(directory, pattern="*.csv", full.names=T)
  data <- lapply(files[id], function(x) read.csv(x, header=T))
  df <- do.call(rbind.data.frame, data)
  
  df.table <- data.frame(c())
  
  for (i in id) {
    s <- sum(complete.cases(df[df$ID == i,]))
    df.table <- rbind(df.table,c(i, s))
  }
  
  names(df.table) <- c("id", "nobs")
  
  df.table$nobs <- as.integer(df.table$nobs)
  
  df.table
}