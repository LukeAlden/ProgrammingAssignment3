best <- function(state, outcome) {
  data <- read.csv("outcome-of-care-measures.csv")
  if(!state %in% data$State) {
    stop("invalid state")
  }
  if(outcome == "heart attack") {
    outcome <- "Heart.Attack"
  } else if(outcome == "heart failure") {
    outcome <- "Heart.Failure"
  } else if(outcome == "pneumonia") {
    outcome <- "Pneumonia"
  } else {
    stop("invalid outcome")
  }
  data <- data[data$State == state,]
  data <- data[order(data$Hospital.Name),]
  mortalities <- data[, paste("Hospital.30.Day.Death..Mortality..Rates.from.", outcome, sep = "")]
  suppressWarnings(mortalities <- as.numeric(as.character(mortalities)))
  as.character(data[which.min(mortalities), "Hospital.Name"])
}

rankhospital <- function(state, outcome, num) {
  data <- read.csv("outcome-of-care-measures.csv")
  if(!state %in% data$State) {
    stop("invalid state")
  }
  if(outcome == "heart attack") {
    outcome <- "Heart.Attack"
  } else if(outcome == "heart failure") {
    outcome <- "Heart.Failure"
  } else if(outcome == "pneumonia") {
    outcome <- "Pneumonia"
  } else {
    stop("invalid outcome")
  }
  data <- data[data$State == state,]
  col <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", outcome, sep = "")
  suppressWarnings(data[, col] <- as.numeric(as.character(data[, col])))
  data <- data[order(data[, col] ,
                     data$Hospital.Name),]
  data <- data[!is.na(data[, col]), ]
  if(num =="best") {
    num <- 1
  } else if(num == "worst") {
    num <- length(data[,col])
  }
  as.character(data[num, "Hospital.Name"])
}

rankall <- function(outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv")
  if(outcome == "heart attack") {
    outcome <- "Heart.Attack"
  } else if(outcome == "heart failure") {
    outcome <- "Heart.Failure"
  } else if(outcome == "pneumonia") {
    outcome <- "Pneumonia"
  } else {
    stop("invalid outcome")
  }
  col <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", outcome, sep = "") #column of interest
  suppressWarnings(data[, col] <- as.numeric(as.character(data[, col]))) #conversion allows for proper ordering
  data <- data[!is.na(data[, col]), ] #remove NA's
  result <- data.frame(hospital = character(), state = character()) #create result to be eventually returned
  states <- unique(data$State) 
  states <- states[order(states)] #list of states in alphabetical order
  if(num == "best") {
    tempnum <- 1
  }
  for(state in states) {
    temp <- data[data$State == state, ] #temporary data for state of interest
    temp <- temp[order(temp[, col], temp[, "Hospital.Name"]), ] #order temp dataframe
    if(num == "worst") {
      tempnum <- length(temp[, col])
    }
    newrow <- data.frame(hospital = temp[tempnum, "Hospital.Name"], state = state)
    result <- rbind(result, newrow)
  }
  result
}
