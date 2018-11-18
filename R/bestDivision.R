bestDivision <-
function(xParts,toUse,toFill,MIN,MAX,noBegin,usedPart)  {

  
  # Here is the return arguments list return(list(xParts=xParts)) 
  
  #LEAST=1 
  #MOST=2 
  
  #MIN=1 
  #MAX=2 
  
  #phase 1 
  
  toDo=list()
  
  toDo[[1]]=c()
  
  temp=c()
  temp$add<-2
  temp$doAdd <- TRUE 
  temp$makeDiff <- TRUE 
  temp$goToOk <- 1 
  temp$goToNotOk <- 2 
  toDo[[1]]=temp
  
  

  #phase 2 
  temp=c()
  temp$add <- cbind(1,2);#Least Most 
  temp$doAdd <- TRUE 
  temp$makeDiff <- TRUE 
  temp$goToOk <- 2 
  temp$goToNotOk <- 3 
  toDo[[2]]<-temp
  
  #phase 3 
  temp=c()
  temp$add <- cbind(2,1,2);#Most Least Most BEGIN NEW 
  temp$doAdd <- FALSE 
  # toDo{3}.makeDiff=; 
  temp$goToOk <- 2 
  temp$goToNotOk <- 4 
  toDo[[3]]<-temp
  
  #phase 4 
  temp=c()
  temp$add <- cbind(1,2);#Least Most BEGIN 
  temp$doAdd <- FALSE 
  # toDo{5}.makeDiff=; 
  temp$goToOk <- 4 
  temp$goToNotOk <- 5 
  toDo[[4]]<-temp
  
  #phase 5 
  temp=c()
  temp$add <- 1;#Least 
  temp$doAdd <- TRUE 
  temp$makeDiff <- TRUE 
  temp$goToOk <- 5 
  temp$goToNotOk <- 6 
  toDo[[5]]<-temp
  
  #phase 6 
  temp=c()
  temp$add <- 2;#Most 
  temp$doAdd <- TRUE 
  temp$makeDiff <- FALSE 
  temp$goToOk <- 6 
  temp$goToNotOk <- 7 
  toDo[[6]]<-temp
  
  #phase 7 
  temp$add <- 2;#Most BEGIN 
  temp$doAdd <- FALSE 
  temp$makeDiff <- FALSE 
  temp$goToOk <- 6 
  # toDo{7}.goToNotOk=; 
  toDo[[7]]<-temp
  
  prevMost <- -1 
  while (sum(toUse) > 0)  { 
    mostLeastData <- mostLeast(toUse) 
    if (prevMost != mostLeastData$whichMost)  { 
      phase <- 1 
      prevMost <- mostLeastData$whichMost 
    } 
    
    currentFill=c()
    currentFill$toFill<-toFill
    currentFill$toUse<-toUse  
    
    output <- tryAdd(xParts,toDo[[phase]],mostLeastData,currentFill,noBegin,MIN,MAX,usedPart) 
    
    
    currentFill<-output$currentFill    
    toUse<-currentFill$toUse
    toFill<-currentFill$toFill
      
    
    xParts<-output$xParts
    phase<-output$nextPhase
    usedPart<-output$usedPart

    # for j=1:length(xParts) 
    # xParts{j} 
    # end 
    # phase 
    # toUse 
    # toFill 
    # pause(0.1) 
  } 
  
  
  bestDivision<-xParts
}
