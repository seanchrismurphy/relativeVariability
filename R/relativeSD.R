# I've updated the relativeSD function to be resilient to missing values. 

relativeSD <-
function(X,MIN,MAX){
  M<-mean(X, na.rm = TRUE)
  
  # Add handling for is all values are NA
  if (is.na(M)) {
    checkOutput(M, MIN, MAX)
    return(NA)
  }
  
  SD<-sd(X, na.rm = TRUE)
  checkInput(X,MIN,MAX)
  n<- sum(complete.cases(X))
  mv<-maximumVAR(M,MIN,MAX,n) #compute the maximum possible standard deviation given the mean
  msd<-sqrt(mv)
  if (msd!=0 & !is.na(msd)){
    rsd=SD/msd;#compute the relative std
  }
  else{
    rsd<-NaN
  checkOutput(M,MIN,MAX)
  }
  
  relativeSD<-rsd
  rsd
}
