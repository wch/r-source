% File src/library/datasets/man/sunspot.year.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{sunspot.year}
\docType{data}
\alias{sunspot.year}
\title{Yearly Sunspot Data, 1700--1988}
\description{
  Yearly numbers of sunspots from 1700 to 1988 (rounded to one digit).

  Note that monthly numbers are available as
  \code{\link{sunspot.month}}, though starting slightly later.
}
\usage{
sunspot.year
}
\format{
  The univariate time series \code{sunspot.year} contains 289
  observations, and is of class \code{"ts"}.
}
\source{
  H. Tong (1996)
  \emph{Non-Linear Time Series}. Clarendon Press, Oxford, p.\sspace{}471.
}
\seealso{
  For \emph{monthly} sunspot numbers, see \code{\link{sunspot.month}}
  and \code{\link{sunspots}}.
  
  Regularly updated yearly sunspot numbers are available from
  WDC-SILSO, Royal Observatory of Belgium, at
  \url{http://www.sidc.be/silso/datafiles}
}
\examples{
utils::str(sm <- sunspots)# the monthly version we keep unchanged
utils::str(sy <- sunspot.year)
## The common time interval
(t1 <- c(max(start(sm), start(sy)),     1)) # Jan 1749
(t2 <- c(min(  end(sm)[1],end(sy)[1]), 12)) # Dec 1983
s.m <- window(sm, start=t1, end=t2)
s.y <- window(sy, start=t1, end=t2[1]) # {irrelevant warning}
stopifnot(length(s.y) * 12 == length(s.m),
          ## The yearly series *is* close to the averages of the monthly one:
          all.equal(s.y, aggregate(s.m, FUN = mean), tol = 0.0020))
## NOTE: Strangely, correctly weighting the number of days per month
##       (using 28.25 for February) is *not* closer than the simple mean:
ndays <- c(31, 28.25, rep(c(31,30, 31,30, 31), 2))
all.equal(s.y, aggregate(s.m, FUN = mean))                     # 0.0013
all.equal(s.y, aggregate(s.m, FUN = weighted.mean, w = ndays)) # 0.0017
}
\keyword{datasets}
