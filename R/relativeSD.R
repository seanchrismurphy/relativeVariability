relativeSD <-
function(X,MIN,MAX){
  idx=which(is.na(X)==0)
  X=X[idx]
  M<-mean(X)
  SD<-sd(X)
  checkInput(X,MIN,MAX)
  n<-length(X)
  mv<-maximumVAR(M,MIN,MAX,n) #compute the maximum possible standard deviation given the mean
  msd<-sqrt(mv)
  if (msd!=0){
    rsd=SD/msd;#compute the relative std
  }
  else{
    rsd<-NaN
  checkOutput(M,MIN,MAX)
  }
  
  relativeSD<-rsd  
}
