\name{maximumIPR}
\alias{maximumIPR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to compute the maximum possible interpercentile range.
}
\description{
A function to compute the maximum possible interpercentile range of a timeseries, given a certain mean M, lower measurement bound MIN, upper measurement bound MAX and length of timeseries n. In a sorted time series the lower percentile is on location n1, the upper percentile is on location n2.
}
\usage{
maximumIPR(M, MIN, MAX, n,n1,n2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{M}{
mean
}
  \item{MIN}{
lower bound of measurements
}
  \item{MAX}{
upper bound of measurements
}
  \item{n}{
length of time series
}
  \item{n1}{
the location of the lower percentile
}
  \item{n2}{
the location of the upper percentile
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
maximum possible interquartile range
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
\examples{
M=5
MIN=0
MAX=10
n=100
n1=10
n2=90
y=maximumIPR(M,MIN,MAX,n,n1,n2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
