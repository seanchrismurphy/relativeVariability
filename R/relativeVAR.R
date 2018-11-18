relativeVAR <-
function(X,MIN,MAX){
  idx=which(is.na(X)==0)
  X=X[idx]
  M<-mean(X)
  v<-var(X)
  checkInput(X,MIN,MAX)
  n<-length(X)
  mv<-maximumVAR(M,MIN,MAX,n) #compute the maximum possible standard deviation given the mean
  if (mv!=0){
    rv=v/mv;#compute the relative std
  }
  else{
    rv=NaN
    checkOutput(M,MIN,MAX)
  }
  
  relativeVAR<-rv  
}
