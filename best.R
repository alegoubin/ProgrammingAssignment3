best <- function(state, outcome) {

  
  
  ## Check that outcome is valid, we can do this before loading the file
  if(!outcome %in% c("heart attack","pneumonia","heart failure"))
  {
    stop("invalid outcome")
  }
  
  ## Read outcome data
  outMeasures <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  ## Check that state is valid
  if(!state %in% outMeasures$State)
  {
    stop("invalid state")
  }
  
  ## Return column ID to use to select data
  idxCase <-match(outcome,c("heart attack","pneumonia","heart failure") )
  columnID <- na.omit(if(idxCase == 1) {11} else if(idxCase == 2) {23} else {17})

  ## Remove na case for selected column
  outMeasures[,columnID] <- suppressWarnings(as.numeric(outMeasures[,columnID]))
  outMeasures <- outMeasures[complete.cases(outMeasures[,columnID]),]
  
  ## Pick only data for selected state
  outMeasures <- outMeasures[outMeasures$State == state,]
  
  #get the best hospital with lowest 30-day death
  return(outMeasures[order(outMeasures[,columnID]),]$Hospital.Name[1])
  
  }

