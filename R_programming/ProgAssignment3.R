file1 = "C:\\Users\\Gannon_chef\\Documents\\Pradeepta\\Coursera\\R_programming\\hospital-data.csv"
file2 = "C:\\Users\\Gannon_chef\\Documents\\Pradeepta\\Coursera\\R_programming\\outcome-of-care-measures.csv"


outcome.data <- read.csv(file2, colClasses = "character")
#head(outcome.data)
#str(outcome.data)

#histogram
outcome.data[, 11] <- as.numeric(outcome.data[, 11])
hist(outcome.data[, 11])

## Get data we're interested in
outcome.data.needed <- as.data.frame(cbind(outcome.data[, 2],   # hospital
                                           outcome.data[, 7],   # state
                                           outcome.data[, 11],  # heart attack
                                           outcome.data[, 17],  # heart failure
                                           outcome.data[, 23]), # pneumonia
                       stringsAsFactors = FALSE)

## Rename columns
colnames(outcome.data.needed) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")


hosp_sort <- function(state,outcome){
  outcome2 <- outcome.data.needed[(outcome.data.needed[, "state"] == state), 
  min_row <- which(as.numeric(outcome2[ ,outcome]) == 
                     min(as.numeric(outcome2[ ,outcome]), na.rm = TRUE))
  hospitals <- outcome2[min_row,"hospital"]
  hospitals <- sort(hospitals)
  hospitals
}

best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  if (!state %in% unique(outcome.data.needed$state))
    stop("Invalid State")
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia"))
    stop("Invalid Outcome")
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  order_selected <- hosp_sort(state,outcome)
  order_selected[1]
  
}

rankhospital <- function(state, outcome, num = 'best') {
  if(!state %in% outcome.data.needed[,"state"]){
    stop('invalid state')
  }
  
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  }
  
  ## Get only the hospitals in chosen state
  outcome2 <- outcome.data.needed[(outcome.data.needed[, "state"] == state), ]

  
  ## Convert outcome rate to numberic, gets a warning
  outcome2[, outcome] <- as.numeric(outcome2[, outcome])
  
  ## Remove NA values
  outcome2 <- outcome2[!is.na(outcome2[, outcome]), ]
  
  ## convert num argument to valid rank
  if(num == "best") {
    num <- 1 
  }
  
  if (num == "worst") {
    num <- nrow(outcome2) 
  }
  
  ## Order by outcome rate
  outcome2 <- outcome2[order(outcome2[, outcome], outcome2[, "hospital"]), ]
  
  ## Get names of hospital 
  outcome2[num, "hospital"]
}

rankall <- function(outcome, num = 'best') {
  hRank <- data.frame()
  for(state in sort(unique(outcome.data.needed[,"state"]))){
    hName<-rankhospital(state, outcome, num)
    hRank <- rbind(hRank,
                   data.frame(hospital = hName,
                              state = state))
  }
  
  ## Return dataframe
  hRank
  
}