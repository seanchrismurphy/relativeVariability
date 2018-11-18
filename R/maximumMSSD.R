maximumMSSD <-
function(MEAN,MIN,MAX,nParts)  {
  # Here is the return arguments list return(list(maxTotal=maxTotal,bestPart=bestPart)) 
  # pwd 
  # addpath('mssd'); 
  N <- sum(nParts) 
  
  turned <- 0 
  if (abs(MIN) > abs(MAX)){#mirror for special cases like MIN = -INF)   
      MINt <- -MAX 
      MAX <- -MIN 
      MIN <- MINt 
      MEAN <- -MEAN 
      turned <- 1 
} 


output<- maxMssdGetN(MEAN,N,MIN,MAX) 
nMin<-output$nMin
nMax<-output$nMax
nMiddle<-output$nMiddle
MIDDLE<-output$m


maxTotal <- 0 



if (nMiddle > 0)  { 
  for (i in (1:(length(nParts)))) { 
    #try LmL 
    xParts=list()
    usedPart=rep(0,length(nParts))
    toUse <- c(nMin,nMax) 
    toFill <- nParts 
    if (nParts[i] >= 3 && nMin >= 2)  { 
      xParts[[i]] <- t(c(MIN,MIDDLE,MIN) )
      usedPart[i]<-1
      toFill[i] <- toFill[i]-3 
      toUse <- toUse-c(2,0) 
      noBegin <- rep(0,length(nParts))
      xParts <- bestDivision(xParts,toUse,toFill,MIN,MAX,noBegin,usedPart) 
       output<- mssdParts(xParts) 
      mssdSum<-output$mssdSum
      mssdMean<-output$mssdMean
      numberOfDifs<-output$numberOfDifs
      if (mssdSum > maxTotal)  { 
        maxTotal <- mssdSum 
        bestPart <- xParts 
      } 
    } 
    
    
    # for j=1:length(xParts) 
    # xParts{j} 
    # end 
    
    
    #try HmH 
    xParts=list()
    usedPart=rep(0,length(nParts))
    toUse <- c(nMin,nMax) 
    toFill <- nParts 
    if (nParts[i] >= 3 && nMax >= 2)  { 
      xParts[[i]] <- t(c(MAX,MIDDLE,MAX) )
      usedPart[i]<-1
      toFill[i] <- toFill[i]-3 
      toUse <- toUse-c(0,2) 
      noBegin <- rep(0,length(nParts))
      xParts <- bestDivision(xParts,toUse,toFill,MIN,MAX,noBegin,usedPart)  
      output<- mssdParts(xParts) 
      mssdSum<-output$mssdSum
      mssdMean<-output$mssdMean
      numberOfDifs<-output$numberOfDifs
      if (mssdSum > maxTotal)  { 
        maxTotal <- mssdSum 
        bestPart <- xParts 
      } 
    } 
    
    # for j=1:length(xParts) 
    # xParts{j} 
    # end 
    #try LmH 
    xParts=list()
    usedPart=rep(0,length(nParts))
    toUse <- c(nMin,nMax) 
    toFill <- nParts 
    if (nParts[i] >= 3 && nMax >= 1 && nMin >= 1)  { 
      xParts[[i]] <- t(c(MAX,MIDDLE,MIN)) 
      usedPart[i]<-1
      toFill[i] <- toFill[i]-3 
      toUse <- toUse-c(1,1) 
      noBegin <- rep(0,length(nParts))
      xParts <- bestDivision(xParts,toUse,toFill,MIN,MAX,noBegin,usedPart)  
      output<- mssdParts(xParts) 
      mssdSum<-output$mssdSum
      mssdMean<-output$mssdMean
      numberOfDifs<-output$numberOfDifs
      if (mssdSum > maxTotal)  { 
        maxTotal <- mssdSum 
        bestPart <- xParts 
      } 
    } 
    
    
    #       for j=1:length(xParts) 
    # xParts{j} 
    # end 
    
    #try mH 
    xParts=list()
    usedPart=rep(0,length(nParts))
    toUse <- c(nMin,nMax) 
    toFill <- nParts 
    if (nParts[i] >= 2 && nMax >= 1)  { 
      xParts[[i]] <- t(c(MIDDLE,MAX)) 
      usedPart[i]<-1
      toFill[i] <- toFill[i]-2 
      toUse <- toUse-c(0,1) 
      noBegin <- rep(0,length(nParts))
      noBegin[i] <- 1 
      xParts <- bestDivision(xParts,toUse,toFill,MIN,MAX,noBegin,usedPart)  
      output<- mssdParts(xParts) 
      mssdSum<-output$mssdSum
      mssdMean<-output$mssdMean
      numberOfDifs<-output$numberOfDifs
      if (mssdSum > maxTotal)  { 
        maxTotal <- mssdSum 
        bestPart <- xParts 
      } 
    } 
    
    
    
    #       for j=1:length(xParts) 
    # xParts{j} 
    # end 
    
    #try mL 
    xParts=list()
    usedPart=rep(0,length(nParts))
    toUse <- c(nMin,nMax) 
    toFill <- nParts 
    if (nParts[i] >= 2 && nMin >= 1)  { 
      xParts[[i]] <- t(c(MIDDLE,MIN)) 
      usedPart[i]<-1
      toFill[i] <- toFill[i]-2 
      toUse <- toUse-c(1,0) 
      noBegin <- rep(0,length(nParts))
      noBegin[i] <- 1 
      xParts <- bestDivision(xParts,toUse,toFill,MIN,MAX,noBegin,usedPart)  
      output<- mssdParts(xParts) 
      mssdSum<-output$mssdSum
      mssdMean<-output$mssdMean
      numberOfDifs<-output$numberOfDifs
      if (mssdSum > maxTotal)  { 
        maxTotal <- mssdSum 
        bestPart <- xParts 
      } 
    } 
    
    
    
    #try m 
    xParts=list()
    usedPart=rep(0,length(nParts))
    toUse <- c(nMin,nMax) 
    toFill <- nParts 
    if (nParts[i] == 1 && nMiddle >= 1)  { 
      xParts[[i]] <- t(c(MIDDLE)) 
      usedPart[i]<-1
      toFill[i] <- toFill[i]-1 
      toUse <- toUse-c(0,0) 
      noBegin <- rep(0,length(nParts))
      noBegin[i] <- 1 
      xParts <- bestDivision(xParts,toUse,toFill,MIN,MAX,noBegin,usedPart)  
      output<- mssdParts(xParts) 
      mssdSum<-output$mssdSum
      mssdMean<-output$mssdMean
      numberOfDifs<-output$numberOfDifs
      if (mssdSum > maxTotal)  { 
        maxTotal <- mssdSum 
        bestPart <- xParts 
      } 
    } 
    
    
    
    # for j=1:length(xParts) 
    # xParts{j} 
    # end 
    
    
  } 
} else {
  xParts=list()
  usedPart=rep(0,length(nParts))
  toUse <- c(nMin,nMax) 
  toFill <- nParts  
  noBegin <- rep(0,length(nParts))
  
  xParts <- bestDivision(xParts,toUse,toFill,MIN,MAX,noBegin,usedPart)  
  output<- mssdParts(xParts) 
  mssdSum<-output$mssdSum
  mssdMean<-output$mssdMean
  numberOfDifs<-output$numberOfDifs
  maxTotal <- mssdSum 
  bestPart <- xParts 
} 

if (turned == 1)  { 
  for (i in (1:(length(bestPart)))) { 
    bestPart[[i]] <- -bestPart[[i]] 
  } 
} 
maxTotal <- maxTotal/ c(N-length(nParts))
# For robustness c() added to divide, Assume you divide by a scalar, not matrix 
# print(bestPart)
maximumMSSD<-maxTotal
}
