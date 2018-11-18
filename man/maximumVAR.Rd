\name{maximumVAR}
\alias{maximumVAR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to compute the maximum possible variance.
}
\description{
A function to compute the maximum possible variance of a timeseries, given a certain mean M, lower measurement bound MIN, upper measurement bound MAX and length of timeseries n.
}
\usage{
maximumVAR(M, MIN, MAX, n)
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
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
maximum possible variance
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
y=maximumVAR(M,MIN,MAX,n)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
