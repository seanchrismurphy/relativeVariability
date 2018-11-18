relativeRMSSD <-
function(X,MIN,MAX)  {
  # Here is the return arguments list return(list(rv=rv,mv=mv)) 
  
  idxNoNan <- which( is.nan(X) == 0 ) 
  
  M <- mean(X[idxNoNan])
  v <- RMSSD(X) 
  checkInput(X[idxNoNan],MIN,MAX) 
  
  addTemp <- 0 
  nPartAdd <- 1 
  nParts<-c()
  for (i in (1:(length(X)))) { 
    if (is.nan(X[i]) == 0)  { 
      addTemp <- addTemp + 1 
    }
    else if (addTemp > 0)  { 
      nParts[nPartAdd] <- addTemp 
      nPartAdd <- nPartAdd + 1 
      addTemp <- 0 
    } 
  } 
  
  if (addTemp > 0)  { 
    nParts[nPartAdd] <- addTemp 
  } 
  
  #   print(X)
  #   print(nParts)
  mv <- maximumRMSSD(M,MIN,MAX,nParts);#compute the maximum possible standard deviation given the mean 
  if (mv != 0)  { 
    rv <- v/mv;#compute the relative mssd 
  } else { 
    rv <- NaN 
    checkOutput(M,MIN,MAX) 
  } 
  
  relativeRMSSD<-rv
}
