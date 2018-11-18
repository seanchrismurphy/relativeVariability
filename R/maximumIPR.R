maximumIPR <-
function(M,MIN,MAX,n,n1,n2)  {
  
  #maximum given the mean M 
  #extreme cases 
  if ( M == MIN || M == MAX )  { 
    mv <- 0 
    
    #normal case 
  } else { 
    if (abs(MIN) > abs(MAX)) {#mirror for special cases like MIN = -INF)   
        MINt <- -MAX 
        MAX <- -MIN 
        MIN <- MINt 
        M <- -M 
  } 
  
  if (n*M < (n2-1) %*% MIN + (n-n2 + 1) %*% MAX)  { 
    mv <- n*(M-MIN)/ c(n-n2 + 1)}
    # For robustness c() added to divide, Assume you divide by a scalar, not matrix 
  else if  (n*M<=n1*MIN+(n-n1)*MAX) { 
      mv <- MAX-MIN 
     } 
  else{ 
        mv <- n*(MAX-M)/n1 
    } 
  }     
}
