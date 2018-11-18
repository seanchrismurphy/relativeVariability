maxMssdGetN <-
function(M,n,MIN,MAX)  {
  
  
  #maximum given the mean M 
  #extreme cases 
  if ( M == MIN )  { 
    nMin <- n 
    nMiddle <- 0 
    nMax <- 0 
    m <- 0 }
  else if ( M == MAX)  { 
      nMax <- n 
      nMiddle <- 0 
      nMin <- 0 
      m <- 0 
      #normal case 
  }else{
      nMax <- floor((n*M-n*MIN)/ c(MAX-MIN));#compute nb
      # For robustness c() added to divide, Assume you divide by a scalar, not matrix 
      nMin <- n-1-nMax;#compute na 
      if (nMax == 0)  { 
        MAX <- 0 
      } 
      nMiddle <- 1 
      m <- n*M-nMin %*% MIN-nMax %*% MAX;#compute m 
    } 
  
  output=c()
  output$nMin<-nMin
  output$nMax<-nMax
  output$nMiddle<-nMiddle
  output$m<-m
  
  maxMssdGetN<-output
  
}
