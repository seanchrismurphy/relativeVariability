checkInput <-
function( X,MIN,MAX )
{
  if (length(which(X>MAX))>0){
    stop('Values found bigger than the maximum')
  }
  else if(length(which(X<MIN))>0){
    stop('Error, MIN>MAX')
  }
  else if(MIN>MAX){
    stop('Error, MIN>MAX')
  }
}
