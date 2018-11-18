relativeRANGE <-
function(X,MIN,MAX)  {
  idx=which(is.na(X)==0)
  X=X[idx]
  M <- mean(X)
  # mean(A) takes the average of the whole matrix, If you need column-wise mean, use colMeans(A) 
  r <- RANGE(X) 
  checkInput(X,MIN,MAX) 
  n <- length(X) 
  n1 <- 1 
  n2 <- n 
  mv <- maximumIPR(M,MIN,MAX,n,n1,n2);#compute the maximum possible iqr given the mean 
  if (mv != 0)  { 
    rv <- r/mv;#compute the relative std 
  } else { 
    rv <- NaN 
    checkOutput(M,MIN,MAX) 
  } 
  
  relativeRANGE<-rv  
}
