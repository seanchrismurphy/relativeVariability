\name{IPR}
\alias{IPR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to compute the interquartile range of a vector
}
\description{
A function to compute the interquartile range of a vector, using percentiles p1 and p2.
}
\usage{
IPR(X, p1, p2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
A vector
}
  \item{p1}{
lower percentile
}
  \item{p2}{
upper percentile
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
the interpercentile range
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
x=c(1,2,3,4,5,6,7,8)
p1=0.25
p2=0.5
IPR(x,p1,p2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
