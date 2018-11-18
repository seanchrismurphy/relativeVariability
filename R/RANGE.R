RANGE <-
function(X)  {
  idx=which(is.na(X)==0)
  X=X[idx]
  # Here is the return arguments list return(list(r=r)) 
  r <- max(X)-min(X)
  # min(A) takes the max of the whole matrix, If you need column-wise mean, use apply(A,2,min)
  # max(A) takes the max of the whole matrix, If you need column-wise mean, use apply(A,2,max) 
  
  RANGE<-r  
}
