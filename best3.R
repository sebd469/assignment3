## best(state,outcome) will find the best outcome from any of the 3 possible outcomes
## for a specific state
## state is a 2 letter code
## reading from outcome-of-care-measures.csv where:
## column 2 is hospital name
## column 7 is state
## columns 11,17 and 23 are repective rates of: heart attack, heart fialure and pneumonia


best<- function(state, outcome){
  
  valid_outcomes=c("heart attack","heart failure", "pneumonia")
  
  # check outcome is valid
  if (!outcome%in%valid_outcomes){
    stop("Invalid outcome")
  }
  
  outcome_col=c(11,17,23) 
  #3 columns numbers of outcome in our file
  #11 for heart attack, 17 for heart failure and 23 for pneumonia
  #my_col_number will contain the column number of outcome selected
  
  L= valid_outcomes==outcome #Logical vector to find position of outcome column
  my_col_number=outcome_col[L] # this is the column outcome to be used
  
  #load data from file into data frame outcome_file
  outcome_file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  
  # check if state valid
  if (!state%in%outcome_file$State){
    stop("Invalid state")
  }
  
  
  y=outcome_file[,c(2,7,my_col_number)] #only keep relevant columns store into y
  #in y column 1 is hospital name, column 2 is state, column 3 is outocme rate
  y=y[y[,2]==state,] # only keep rows relevant to state parameter
  y[,3]= as.numeric(y[,3]) #coerce my outcome column to be numeric
  y = na.omit(y)
  names(y)<-c("hospital","state","rate")
  y=y[order(y$rate,y$hospital),] # order rows by rate then hospital name
  
  y[1,1]
  
  
  }