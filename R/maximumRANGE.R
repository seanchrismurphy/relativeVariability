maximumRANGE <-
function(M,MIN,MAX,n)  {
  # mean(A) takes the average of the whole matrix, If you need column-wise mean, use colMeans(A) 
  n1 <- 1 
  n2 <- n 
  mv <- maximumIPR(M,MIN,MAX,n,n1,n2);#compute the maximum possible iqr given the mean 
  maximumRANGE<-mv
}
