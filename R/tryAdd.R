tryAdd <-
function(xParts,toDo,mostLeastData,currentFill,noBegin,MIN,MAX,usedPart)  {
  # Here is the return arguments list return(list(xParts=xParts,toUse=toUse,toFill=toFill,nextPhase=nextPhase)) 

  chooseFrom <- c(MIN,MAX) 
  if (mostLeastData$whichMost == 1){ #most left = MIN = 1 useMost = useMin 2-- > 1)  { 
      toAddMaxMin <- -1*toDo$add + 3 
} else { 
  toAddMaxMin <- toDo$add#most left <- MAX <- 2 useMost <- useMax 2-- > 2 
} 


toAdd <- chooseFrom[toAddMaxMin] 



done <- 0 #to test if something is added 


if (currentFill$toUse[1] >= length(which((toAddMaxMin == 1)))&& currentFill$toUse[2] >= length(which((toAddMaxMin == 2)))){  # enough resources)   
    for (i in (1:(length(currentFill$toFill)))) {#check all parts 
      if (currentFill$toFill[i] >= length(toAdd)){#enough space in specific part)   
          if (toDo$doAdd){ # we need to add to part)   
              if (usedPart[i]>0) {
                if (toDo$makeDiff){ #extra part must make MINMAX jump with existing part)  
                    if ((xParts[[i]][length(xParts[[i]])])!=toAdd[1] ){
                        xParts[[i]] <- cbind(xParts[[i]],t(toAdd)) 
                        done <- 1 
                      currentFill <- updateUseFill(toAddMaxMin,currentFill$toUse,currentFill$toFill,i) 
                        break }
                     else if (xParts[[i]][1]!=toAdd[1] && noBegin[i]==0){
                              xParts[[i]] <- cbind(t(toAdd[seq(from = length(toAdd),to = 1,by = -1)]),xParts[[i]]) 
                              done <- 1 
                              currentFill <- updateUseFill(toAddMaxMin,currentFill$toUse,currentFill$toFill,i) 
                              break 
                    } 
                } else { #extra part must not make a MINMAX jump with existing part 
                xParts[[i]] <- cbind(xParts[[i]],t(toAdd)) 
                done <- 1 
                currentFill <- updateUseFill(toAddMaxMin,currentFill$toUse,currentFill$toFill,i) 
                break 
              } 
            }
          }
          else { #we need to start new part 
            if (usedPart[i] == 0)  { 
              usedPart[i]=1
              xParts[[i]] <- t(toAdd)
              done <- 1 
              currentFill <- updateUseFill(toAddMaxMin,currentFill$toUse,currentFill$toFill,i) 
              break 
            } 
          } 
        } 
  } 
} 

if (done == 1)  { #is something added 
    nextPhase <- toDo$goToOk 
    } else { 
      nextPhase <- toDo$goToNotOk 
    }
output<-c()


output$xParts<-xParts
output$currentFill<-currentFill
output$nextPhase<-nextPhase
output$usedPart<-usedPart
tryAdd<-output
}
