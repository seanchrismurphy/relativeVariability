MSSD <-
function(X)  {
    add <- 0 
  total <- 0 
  for (i in (1:(length(X)-1))) { 
                              if (is.nan(X[i]) == 0 && is.nan( X[i + 1] ) == 0)  { 
                                total <- total + (X[i]-X[i + 1])^2 
                                add <- add + 1 
                              } 
  } 
  
  total <- total/add
  # For robustness c() added to divide, Assume you divide by a scalar, not matrix 
  
  MSSD<-total
}
