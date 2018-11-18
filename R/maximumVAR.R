maximumVAR <-
function(M,MIN,MAX,n){
  # check input

  #extreme cases
  if (M==MIN || M==MAX){
    mv=0;
  }
  # normal case
  else{
    
    if(abs(MIN)>abs(MAX)){ #mirror for special cases like MIN=-INF
    MINt<--MAX
    MAX<--MIN
    MIN<-MINt            
    M<--M
    }
    
  nMax<-floor((n*M-n*MIN)/(MAX-MIN)) #compute nb  
  nMin<-n-1-nMax # compute na
  
  if(nMax==0){
    MAX<-0
  }
  
  m<-n*M-nMin*MIN-nMax*MAX # compute m
  mv<-(nMin*(MIN-M)^2+nMax*(MAX-M)^2+(M-m)^2)/(n-1) #compute maximum variability
  }
  
  maximumVar=mv
}
