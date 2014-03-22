% File src/library/datasets/man/sunspot.month.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{sunspot.month}
\docType{data}
\alias{sunspot.month}
\title{Monthly Sunspot Data, from 1749 to "Present"}
\description{
  Monthly numbers of sunspots, as from the World Data Center, aka SIDC.
  This is the version of the data that will occasionally be updated when
  new counts become available.%% ~/R/MM/MISC/read-sunspots.R
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
  \code{sunspot.month} is a longer version of \code{\link{sunspots}};
  the latter runs until 1983 and is kept fixed (for reproducibility as example
  dataset).
}
\source{
  WDC-SILSO, Solar Influences Data Analysis Center (SIDC),
  Royal Observatory of Belgium, Av. Circulaire, 3, B-1180 BRUSSELS
  Currently at \url{http://www.sidc.be/silso/datafiles}% was http://sidc.oma.be/sunspot-data/
}
\author{
  R
}
\examples{
require(stats); require(graphics)
## Compare the monthly series
plot (sunspot.month,
      main="sunspot.month & sunspots [package'datasets']", col=2)
lines(sunspots) # -> faint differences where they overlap

## Now look at the difference :
all(tsp(sunspots)     [c(1,3)] ==
    tsp(sunspot.month)[c(1,3)]) ## Start & Periodicity are the same
n1 <- length(sunspots)
table(eq <- sunspots == sunspot.month[1:n1]) #>  132  are different !
i <- which(!eq)
rug(time(eq)[i])
s1 <- sunspots[i] ; s2 <- sunspot.month[i]
cbind(i = i, time = time(sunspots)[i], sunspots = s1, ss.month = s2,
      perc.diff = round(100*2*abs(s1-s2)/(s1+s2), 1))
}
\keyword{datasets}
