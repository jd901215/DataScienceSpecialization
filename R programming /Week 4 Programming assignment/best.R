best <- function(state, outcome){
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  
  csv_outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  states <- csv_outcomes[!duplicated(csv_outcomes[, "State"]), "State"]

  if (!is.element(state, states)){
    stop("invalid state")
  }

  else if (identical(outcome, "heart attack")) {
    disease <- "Heart.Attack"
  }
  else if (identical(outcome, "heart failure")) {
    disease <- "Heart.Failure"
  }
  else if (identical(outcome, "pneumonia")) {
    disease <- "Pneumonia"
  }
  else {
    stop("invalid outcome")
  }
  
  full_name_outcome <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", disease, sep = "")
  
  df <- data.frame(split(csv_outcomes[c( full_name_outcome, "Hospital.Name")], csv_outcomes[, "State"])[state])
  
  name_disease <- paste(state, ".",full_name_outcome, sep = "")
  name_hospital <- paste(state, ".", "Hospital.Name", sep = "")
  
  z <- df[order(df[,name_disease], df[, name_hospital]), ]
  print(head(z))
  z[1,2]
  
}