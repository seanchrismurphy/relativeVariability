relativeMSSD <-
function(X,MIN,MAX)  {
  # Here is the return arguments list return(list(rv=rv,mv=mv)) 
  
  # So as not to break further stuff, update this expression to handle not just NaNs but also NA and Inf. 
  idxNoNan <- which(!is.nan(X) & !is.na(X) & is.finite(X)) 
  
  M <- mean(X[idxNoNan])
  v <- MSSD(X) 
  checkInput(X[idxNoNan],MIN,MAX) 
  
  # Add handling for is all values are NA
  if (is.na(M)) {
    checkOutput(M, MIN, MAX)
    return(NA)
  }
  
# Again remove loop and use builtin.
  nParts <- sum(!is.na(X))
#   print(X)
#   print(nParts)
  mv <- maximumMSSD(M,MIN,MAX,nParts);#compute the maximum possible standard deviation given the mean 
  
  if (mv != 0 & !is.na(mv))  { 
    rv <- v/mv;#compute the relative mssd 
  } else { 
    rv <- NaN 
    checkOutput(M,MIN,MAX) 
  } 
  
  relativeMSSD<-rv
  return(relativeMSSD)
}
