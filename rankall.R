## The rankall function takes 2 arguments: outome and num
## It returns a 2-column data frame containing the hospital in each state 
##that has the ranking specifed in num.

rankall <- function(outcome, num = "best"){
  
  valid_outcomes=c("heart attack","heart failure", "pneumonia")
  
  
  # check outcome is valid
  if (!outcome%in%valid_outcomes){
    stop("Invalid outcome")
  }
  
  # check num is a valid rank: best, worst or numeric
  
  if (is.numeric(num)) num<-as.integer(num)
  else {
    if (num=="best") num<-1L
    else{
      if (num!="worst") stop("Invalid rank")
      
    }
  }
  pos<-num # to be used for my ranking
  
## reading the data  
  outcome_col=c(11,17,23) 
  #3 columns numbers of outcome in our file
  #11 for heart attack, 17 for heart failure and 23 for pneumonia
  #my_col_number will contain the column number of outcome selected
  
  L<- valid_outcomes==outcome #Logical vector to find position of outcome column
  my_col_number=outcome_col[L] # this is the column outcome to be used
  
  #load data from file into data frame outcome_file
  outcome_file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  y=outcome_file[,c(2,7,my_col_number)] #only keep relevant columns store into y
  #in y column 1 is hospital name, column 2 is state, column 3 is outocme rate
  
  
##cleaning and sorting the data  
  y[,3]= as.numeric(y[,3]) #coerce my outcome column to be numeric
  y = na.omit(y) # remove na values
  y= y[order(y[,2],y[,3], y[,1]),] #order by states then rate then hospital name
  mylist =split(y,y[,2]) #split by STATE
  states=names(mylist) # vector containing all the different state codes
  results=character(0) # vector to contain the hospital name
  
  # loop around the states and find hospital at rank m each time
  for (i in seq_along(states)){
    if(num=="worst") pos<-length(mylist[[i]][,1])#last postion
    
    results[i]=mylist[[i]][pos,1]  #find the hospital at rank pos for ith state
  }
  
  df=data.frame(results,states)
  names(df)<- c("hospital","state")
  df
  
  
}