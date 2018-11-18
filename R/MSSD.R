MSSD <-
function(X)  {
  
  
  # Remove inefficient loop and instead use the built in, vectorised diff function, which takes the successive differences. 
  differences <- diff(X)
  differences <- differences[!is.na(differences)]
  
  differences <- differences ^ 2
  
  MSSD <- mean(differences)
  
  # Add a return statement so we actually get back the value we're looking for without having to assign it. 
  return(MSSD)
}
