\name{sunspot}
\docType{data}
%\alias{sunspot}
\alias{sunspot.month}
\alias{sunspot.year}
\title{
  Yearly Sunspot Data, 1700--1988, and
  Monthly Sunspot Data, 1749--1997
}
\description{
  Monthly and yearly number of sunspots. 
}
\usage{
sunspot.month
sunspot.year
}
\format{
  The univariate time series \code{sunspot.year} and
  \code{sunspot.month} contain 289 and 2988 observations, respectively.
  The objects are of class \code{"ts"}.
}
\seealso{
  \code{sunspot.month} is a longer version of \code{\link{sunspots}}
  that runs until 1988 rather than 1983.
}
\source{
  Monthly data: Sunspot Index Data Center,
  World Data Center-C1 For Sunspot Index
  Royal Observatory of Belgium, Av. Circulaire, 3, B-1180 BRUSSELS
  \url{http://www.oma.be/KSB-ORB/SIDC/sidc_txt.html}

  Yearly data: H. Tong (1996)
  \emph{Non-Linear Time Series}. Clarendon Press, Oxford, p. 471. 
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\section{Warning}{
  For back-compatibilty these datasets can be copied to the user's
  workspace (or elsewhere) by \code{data(sunspot)}.  However, as package
  \pkg{lattice} has a dataset \code{sunspot} which can be retrieved by
  the same command, that usage is deprecated.
}
\examples{
require(stats)
## Compare the monthly series 
plot (sunspot.month, main = "sunspot.month [stats]", col = 2)
lines(sunspots) # "very barely" see something

## Now look at the difference :
all(tsp(sunspots)     [c(1,3)] ==
    tsp(sunspot.month)[c(1,3)]) ## Start & Periodicity are the same
n1 <- length(sunspots)
table(eq <- sunspots == sunspot.month[1:n1]) #>  132  are different !
i <- which(!eq) 
rug(time(eq)[i])
s1 <- sunspots[i] ; s2 <- sunspot.month[i]
cbind(i = i, sunspots = s1, ss.month = s2,
      perc.diff = round(100*2*abs(s1-s2)/(s1+s2), 1))
}
\keyword{datasets}
