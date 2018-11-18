updateUseFill <-
function(toAddMaxMin,toUse,toFill,i)  {
  # Here is the return arguments list return(list(toUse=toUse,toFill=toFill)) 
  for (j in (1:(length(toAddMaxMin)))) { 
    toUse[toAddMaxMin[j]] <- toUse[toAddMaxMin[j]]-1 
    toFill[i] <- toFill[i]-1 
  } 
  
  currentFill<-list()
  currentFill$toUse<-toUse
  currentFill$toFill<-toFill
  
  updateUseFill<-currentFill
}
