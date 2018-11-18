mssdParts <-
function(xParts)  {

  
  n <- 0 
  output <- 0 
  for (i in (1:(length(xParts)))) { 
    if(length(xParts[[i]])>1){
    for (j in (1:(length(xParts[[i]])-1))) {
    output <- output + (xParts[[i]][j + 1]-xParts[[i]][j])^2 
    n <- n + 1 
    }} 
  } 

mssdSum <- output 
mssdMean <- output/n 
numberOfDifs <- n 


out=list()
out$mssdSum<-mssdSum
out$mssdMean<-mssdMean
out$numberOfDifs<-numberOfDifs
mssdParts<-out

}
