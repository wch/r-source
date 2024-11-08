% File src/library/datasets/man/sunspot.month.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2024 R Core Team
% Distributed under GPL 2 or later

\name{sunspot.month}
\title{Monthly Sunspot Data, from 1749 to "Present"}
\docType{data}
\encoding{UTF-8}
\alias{sunspot.month}
\description{
  Monthly numbers of sunspots, as from the World Data Center, aka \abbr{SIDC}.
  This is the version of the data that will occasionally be updated when
  new counts become available (or the numbers are recalibrated).\cr % ~/R/MM/MISC/read-sunspots.R
  For strict reproducibility, hence use the \code{\link{sunspots}} data instead!
}
\usage{
sunspot.month
}
\format{
  The univariate time series \code{sunspot.year} and
  \code{sunspot.month} contain 289 and
  3177
  observations, respectively, where the latter will increase over time.
  The objects are of class \code{"\link{ts}"}.
}
\seealso{
  \code{sunspot.month} is a more up-to-date and hence also longer version of
  \code{\link{sunspots}};
  the latter runs until 1983 and is kept fixed (for reproducibility as example
  dataset).
}
\source{
  \I{WDC-SILSO}, Solar Influences Data Analysis Center (\abbr{SIDC}),
  Royal Observatory of Belgium, Av. Circulaire, 3, B-1180 BRUSSELS
  Currently at \url{https://www.sidc.be/SILSO/datafiles}% was
  % http://www.sidc.be/silso/datafiles, and http://sidc.oma.be/sunspot-data/

  From around 2015, expert astronomers decided to recalibrate historical sunspot
  numbers, see the \sQuote{References}.
  This is \emph{not yet} visible in current \code{sunspot.month} but
  may well change by the next release of \R.
}
\references{
  Clette, \enc{Frédéric}{Frederic} and \enc{Lefèvre}{Lefevre}, Laure (2016)
  The New Sunspot Number: Assembling All Corrections.
  \emph{Solar Physics} \bold{291}, 2629--2651. \doi{10.1007/s11207-016-1014-y}

  Clette, F.,  \enc{Lefèvre}{Lef`evre}, L.,  Chatzistergos, T. et al. (2023)
  Recalibration of the Sunspot-Number: Status Report.
  \emph{Solar Physics} \bold{298}: 44.  \doi{10.1007/s11207-023-02136-3}
}
\author{
  R Core Team.
}
\examples{
require(stats); require(graphics)
## Compare the monthly series
plot (sunspot.month,
      main="sunspot.month & sunspots [package 'datasets']", col=2)
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

## How to recreate the "old" sunspot.month (R <= 3.0.3):
.sunspot.diff <- cbind(
    i = c(1202L, 1256L, 1258L, 1301L, 1407L, 1429L, 1452L, 1455L,
          1663L, 2151L, 2329L, 2498L, 2594L, 2694L, 2819L),
    res10 = c(1L, 1L, 1L, -1L, -1L, -1L, 1L, -1L,
          1L, 1L, 1L, 1L, 1L, 20L, 1L))
ssm0 <- sunspot.month[1:2988]
with(as.data.frame(.sunspot.diff), ssm0[i] <<- ssm0[i] - res10/10)
sunspot.month.0 <- ts(ssm0, start = 1749, frequency = 12)
stopifnot(length(sunspot.month.0) == 2988)
}
\keyword{datasets}
