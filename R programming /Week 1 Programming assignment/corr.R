corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!

  complete_cases <- complete(directory)
  complete_cases <- complete_cases[complete_cases$nobs > threshold, "id"]

  correlations <- double()
  
  for(csv_index in complete_cases){
    name <- paste("specdata/",toString(sprintf("%03d", csv_index)), ".csv", sep = "")
    temp_csv <- read.csv(name)
    temp_cor <- cor(temp_csv[complete.cases(temp_csv), "sulfate"], temp_csv[complete.cases(temp_csv) , "nitrate"])
    correlations <- c(correlations, temp_cor)
  }
  return(correlations)
}
