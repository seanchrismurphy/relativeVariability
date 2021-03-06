\name{relativeRMSSD}
\alias{relativeRMSSD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to compute the relative Root Squared Successive Difference (RMSSD*)
}
\description{
A function to compute the relative Root Squared Successive Difference (RMSSD*)
}
\usage{
relativeRMSSD(X, MIN, MAX)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
vector including NaN, for example c(1,2,3,NaN,4,5,NaN,1,2). Put a NaN where the time series is interupted.
}
  \item{MIN}{
the lower bound of the measurements
}
  \item{MAX}{
the upper bound of the measurements
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The RMSSD*
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x=c(1,2,3,NaN,4,5,NaN,1,2)
y=relativeRMSSD(x,0,10)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
