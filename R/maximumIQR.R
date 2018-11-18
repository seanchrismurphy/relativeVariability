maximumIQR <-
function(M,MIN,MAX,n)  {
  p1 <- 0.25 
  p2 <- 0.75 
  n1 <- ceiling( n*p1 ) 
  n2 <- ceiling( n*p2 ) 
  mv <- maximumIPR(M,MIN,MAX,n,n1,n2);#compute the maximum possible iqr given the mean 
  maximumIQR<-mv
}
