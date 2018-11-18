\name{relativeVariability-package}
\alias{relativeVariability-package}
\alias{relativeVariability}
\docType{package}
\title{
  \packageTitle{relativeVariability}
}
\description{
  \packageDescription{relativeVariability}
}
\details{
  
  The DESCRIPTION file:
    \packageDESCRIPTION{relativeVariability}
  \packageIndices{relativeVariability}
  ~~ An overview of how to use the package, including the most important functions ~~
}
\author{
  \packageAuthor{relativeVariability}
  Maintainer: \packageMaintainer{relativeVariability}
}
\references{
  ~~ Literature or other references for background information ~~
}
\keyword{ package }
\seealso{
  ~~ Optional links to other man pages, e.g. ~~
    ~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
# use the relative standard deviation
x=c(1,1,3)
rsd=relativeSD(x,0,5)
rsd #the result should be 0.4

# use the relative root mean squared successive difference
x=c(1,2,NaN,3,4)
rrmssd=relativeRMSSD(x,0,5)
rrmssd #the result should be 0.2



################DATA SET EXAMPLE#####################################
data(ESMData)
help(ESMData)

MIN=1 # measurements were bounded between 1 and 100 in this experiment
MAX=100

# compute mean and variability for each participant
ui=unique(ESMData$ID); #unique id's
M=NULL
SD=NULL
RSD=NULL
N=NULL
MSD=NULL
W=NULL
CESD=NULL

for (i in 1:length(ui)){  
  idx=which(ESMData$ID==ui[i]) # location of no nan data of participant ui(i)
  M[i]=mean(ESMData$Sad[idx],na.rm = TRUE) # mean of this data 
  SD[i]=sd(ESMData$Sad[idx],na.rm = TRUE) # standard deviation of this data
  RSD[i]=relativeSD(ESMData$Sad[idx],MIN,MAX) # relative standard deviation 
  N[i]=length(which(ESMData$ID==ui[i] & is.na(ESMData$Sad)==FALSE)) # number of data points
  MSD[i]=maximumSD(M[i],MIN,MAX,N[i]) # maximum possible standard deviation
  CESD[i]=ESMData$CESD[idx[1]]
  W[i]=MSD[i]^2
}


idxUse=which(M>MIN & M <MAX) # exclude particpants with mean equal to one of the bounds and exclude participants with too few data points
W=W/sum(W)*length(idxUse)

# calculate relation bewteen mean and variability measures
rlin1=cor(M[idxUse],SD[idxUse])
rlin2=cor(M[idxUse],RSD[idxUse])
print(rlin1)
print(rlin2)


rexp1=cor(MSD[idxUse],SD[idxUse])
rexp2=cor(MSD[idxUse],RSD[idxUse])
print(rexp1)
print(rexp2)

# plot relation between mean and variability measures
x=seq(MIN,MAX,by=0.1);
y=NULL
for (i in 1:length(x)){
  y[i]=maximumSD(x[i],MIN,MAX,median(N))# %compute theoretical maximum for several means to show on oplot
}
y2=rep(1,length(y)) #theoretical maximum for relative standard deviation

par(mfrow=c(1,2))
plot(M[idxUse],SD[idxUse],xlim=c(MIN,MAX),ylim=c(0,max(y)*1.2),xlab="M",ylab="SD",pch=20,cex= 1)
lines(x,y,col='red')

plot(M[idxUse],RSD[idxUse],xlim=c(MIN,MAX),ylim=c(0,max(y2)*1.2),xlab="M",ylab="SD*",pch=20,cex= 1)
lines(x,y2,col='red')

# the RSD is less dependent on the M. Any analysis that uses the SD will be confounded by the mean M. 

# link variability to CESD      
mdl=lm(CESD[idxUse]~cbind(RSD[idxUse],M[idxUse]))
summary(mdl) # we find a significant linear relation between the relative standard deviation and the CESD score

# do weighted analysis
mdlW=lm(CESD[idxUse]~cbind(RSD[idxUse],M[idxUse]),weights=W[idxUse])
Keff=(length(idxUse)^2)/sum(W^2)
mdlW$df.residual=Keff-3  # degrees of freedom = effective sample size - number of variables - 1
summary(mdlW) # this effect is not too dependent on observations near the bounds.

  
}
