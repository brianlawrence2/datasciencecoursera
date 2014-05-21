pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  files <- list.files(directory, pattern="*.csv", full.names=T)
  data <- lapply(files, function(x) read.csv(x, header=T))
  data <- lapply(data, function(z) z[,1:4])
  df <- do.call(rbind.data.frame, data)
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  df.mean <- mean(df[df$ID %in% id, pollutant],na.rm=T)
  df.mean
}