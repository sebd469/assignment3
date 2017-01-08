# Return hospital at rank m for given state/ outcome
#hospital to be ramked by outcoe rate then alphabetical on hospital name

rankhospital <- function(state, outcome, num="best"){
  
  valid_outcomes=c("heart attack","heart failure", "pneumonia")
  
  
  # check outcome is valid
  if (!outcome%in%valid_outcomes){
    stop("Invalid outcome")
  }
  
  outcome_col=c(11,17,23) 
  #3 columns numbers of outcome in our file
  #11 for heart attack, 17 for heart failure and 23 for pneumonia
  #my_col_number will contain the column number of outcome selected
  
  L<- valid_outcomes==outcome #Logical vector to find position of outcome column
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
  y = na.omit(y) # remove na values
  
  y= y[order(y[,3], y[,1]),] #order by rate then hospital name
   
  # we need to run some checks on num (rank)
  if (num == "best") num<- 1
  if (num =="worst") num<-nrow(y)
  num<-as.integer(num)
  if (is.na(num)){ # incorrect entry
    stop("Invalid rank parameter")
  }
    
  y[num,1] # return name of hospital  with rank m
    
  
  
  
}