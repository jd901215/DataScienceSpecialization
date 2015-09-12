pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  means <- double()
  conc <- double()
  for (csv_index in id){
    name <- paste("specdata/",toString(sprintf("%03d", csv_index)), ".csv", sep = "")
    temp_csv <- read.csv(name)
    conc <- c(conc, temp_csv[ complete.cases(temp_csv[, pollutant]), pollutant])
  }
  mean(conc)
}