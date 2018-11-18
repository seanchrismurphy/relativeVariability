relativeIQR <-
function(X,MIN,MAX)  {
  idx=which(is.na(X)==0)
  X=X[idx]
  M <- mean(X)
  # mean(A) takes the average of the whole matrix, If you need column-wise mean, use colMeans(A) 
  iqr <- IQR(X) 
  checkInput(X,MIN,MAX) 
  n <- length(X) 
  p1 <- 0.25 
  p2 <- 0.75 
  n1 <- ceiling( n*p1 ) 
  n2 <- ceiling( n*p2 ) 
  mv <- maximumIPR(M,MIN,MAX,n,n1,n2);#compute the maximum possible iqr given the mean 
  if (mv != 0)  { 
    rv <- iqr/mv;#compute the relative std 
  } else { 
    rv <- NaN 
    checkOutput(M,MIN,MAX) 
  } 
  relativeIQR<-rv
}
