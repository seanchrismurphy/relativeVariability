\name{RMSSD}
\alias{RMSSD}

\title{
Function to compute Root Mean Squared Successive Difference (RMSSD)
}
\description{
Function to compute Root Mean Squared Successive Difference (RMSSD)
}
\usage{
RMSSD(X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
vector including NaN, for example c(1,2,3,NaN,4,5,NaN,1,2). 
Put a NaN where the time series is interupted.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The RMSSD of X. in the case of x=c(1,2,3,NaN,5,2,NaN,1,1). This is sqrt(((1+1)+(9)+(0))/4)=1.658312
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Mestdagh Merijn
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x=c(1,2,3,NaN,5,2,NaN,1,1)
y=RMSSD(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
