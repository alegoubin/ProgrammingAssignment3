rankall <- function(outcome, num = "best") {
  
  ## Check that outcome is valid, we can do this before loading the file
  if(!outcome %in% c("heart attack","pneumonia","heart failure"))
  {
    stop("invalid outcome")
  }
  
  ## Read outcome data
  outMeasures <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  ## Return column ID to use to select data
  idxCase <-match(outcome,c("heart attack","pneumonia","heart failure") )
  columnID <- na.omit(if(idxCase == 1) {11} else if(idxCase == 2) {23} else {17})
  
  ## Remove na case for selected column
  outMeasures[,columnID] <- suppressWarnings(as.numeric(outMeasures[,columnID]))
  outMeasures <- outMeasures[complete.cases(outMeasures[,columnID]),]
  
  ## Split data by set
  outStateSplit <- split(outMeasures,outMeasures$State)
  
  ## Define function to order and filter data based on requested rank that we will use for each state
  ord <- function(x = data.frame()) 
  {
    
    if (num=="best")
    {
      ## sort ascending and take first vector element
      x<- x[order(x[,columnID],x$Hospital.Name),c("Hospital.Name","State")]
      x<- x[1,]
      
    }
    else if (num=="worst")
    {
      ## sort descending and take first vector element
      x<- x[order(x[,columnID],x$Hospital.Name,na.last = TRUE, decreasing = TRUE),c("Hospital.Name","State")]
      x<- x[1,]
    }
    else 
    {
      ## generate based on selected rank by the selected column and hospital name
      rankNum <- as.numeric(num)
      x<- x[order(x[,columnID],x$Hospital.Name),c("Hospital.Name","State")]
      x<- x[rankNum,]    
    }
    
  }
  
  ## Apply defined ranking function for each state and remerge results when done
  result <- do.call("rbind",lapply(outStateSplit, ord))
  
  ## Change column names
  colnames(result) <- c("hospital","state")
  
  return(result)
}
