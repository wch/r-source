\name{sunspot.month}
\docType{data}
\alias{sunspot.month}
\title{
  Monthly Sunspot Data, 1749--1997
}
\description{
  Monthly numbers of sunspots. 
}
\usage{
sunspot.month
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
  World Data Center-C1 For Sunspot Index
  Royal Observatory of Belgium, Av. Circulaire, 3, B-1180 BRUSSELS
  \url{http://www.oma.be/KSB-ORB/SIDC/sidc_txt.html}
}
\note{
  Prior to \R 2.0.0 \code{sunspot.month} and \code{\link{sunspot.year}}
  were copied to the user's workspace (or elsewhere) by
  \code{data(sunspot)}. However, as package \pkg{lattice} has a dataset
  \code{sunspot} which can be retrieved by the same command, that usage
  has been removed.
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
