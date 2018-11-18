IPR <-
function(X,p1,p2)  {
  idx=which(is.na(X)==0)
  X=X[idx]
  #return the difference between percentile p2 and percentile p1 
  n <- length(X) 
  n1 <- ceiling( p1*n ) 
  if ( n1 < 1 )  { 
    n1 <- 1 
  } 
  
  n2 <- ceiling( p2*n ) 
  if ( n2 < 1 )  { 
    n2 <- 1 
  } 
  xs <- sort(X) 
  v <- xs[n2]-xs[n1] 
  IPR<-v  

}
