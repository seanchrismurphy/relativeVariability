mostLeast <-
function(toUse)  {
  # Here is the return arguments list return(list(mostLeastData=mostLeastData)) 
  
  mostLeastData=c()
  if (toUse[1] > toUse[2])  { 
    mostLeastData$whichMost <- 1 
    mostLeastData$whichLeast <- 2 
    
  } else { 
    mostLeastData$whichMost <- 2 
    mostLeastData$whichLeast <- 1 
  } 
  
  mostLeast<-mostLeastData
}
