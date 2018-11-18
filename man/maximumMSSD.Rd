\name{maximumMSSD}
\alias{maximumMSSD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to compute the maximum possible mean squared successive difference (MSSD)
}
\description{
A function to compute the maximum possible mean squared successive difference of a timeseries, given a certain mean MEAN, lower measurement bound MIN, upper measurement bound MAX and length of timeseries parts n.
}
\usage{
maximumMSSD(MEAN, MIN, MAX, nParts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MEAN}{
the mean of the time series
}
 \item{MIN}{
lower bound of measurements
}
  \item{MAX}{
upper bound of measurements
}
  \item{nParts}{
an array with the lengths of the different uninterupted parts of the time series. 
A time series c(1,2,3,NaN,2,1) means that nParts=c(3,2).
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
maximum possible MSSD
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Merijn Mestdagh
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{M=5
MIN=0
MAX=10
nParts=c(2,5)
y=maximumMSSD(M,MIN,MAX,nParts)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
