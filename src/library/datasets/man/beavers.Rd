% File src/library/datasets/man/beavers.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1994-9 W. N. Venables and B. D. Ripley
% Distributed under GPL 2 or later

\name{beavers}
\docType{data}
\alias{beavers}
\alias{beaver1}
\alias{beaver2}
\title{Body Temperature Series of Two Beavers}
\usage{
beaver1
beaver2
}
\description{
  Reynolds (1994) describes a small part of a study of the long-term
  temperature dynamics of beaver \emph{Castor canadensis} in
  north-central Wisconsin.  Body temperature was measured by telemetry
  every 10 minutes for four females, but data from a one period of
  less than a day for each of two animals is used there.
}
\format{
  The \code{beaver1} data frame has 114 rows and 4 columns on body
  temperature measurements at 10 minute intervals.

  The \code{beaver2} data frame has 100 rows and 4 columns on body
  temperature measurements at 10 minute intervals.

  The variables are as follows:
  \describe{
    \item{day}{Day of observation (in days since the beginning of
      1990), December 12--13 (\code{beaver1}) and November 3--4
      (\code{beaver2}).}
    \item{time}{Time of observation, in the form \code{0330} for
      3:30am}
    \item{temp}{Measured body temperature in degrees Celsius.}
    \item{activ}{Indicator of activity outside the retreat.}
  }
}
\note{
  The observation at 22:20 is missing in \code{beaver1}.
}
\source{
  P. S. Reynolds (1994) Time-series analyses of beaver body
  temperatures.  Chapter 11 of Lange, N., Ryan, L., Billard, L.,
  Brillinger, D., Conquest, L. and Greenhouse, J. eds (1994)
  \emph{Case Studies in Biometry.}
  New York: John Wiley and Sons.
}
%% consider converting times to POSIXct (using 'yday' ?)
\examples{
require(graphics)
(yl <- range(beaver1$temp, beaver2$temp))

beaver.plot <- function(bdat, ...) {
  nam <- deparse(substitute(bdat))
  with(bdat, {
    # Hours since start of day:
    hours <- time \%/\% 100 + 24*(day - day[1]) + (time \%\% 100)/60
    plot (hours, temp, type = "l", ...,
          main = paste(nam, "body temperature"))
    abline(h = 37.5, col = "gray", lty = 2)
    is.act <- activ == 1
    points(hours[is.act], temp[is.act], col = 2, cex = .8)
  })
}
op <- par(mfrow = c(2, 1), mar = c(3, 3, 4, 2), mgp = 0.9 * 2:0)
 beaver.plot(beaver1, ylim = yl)
 beaver.plot(beaver2, ylim = yl)
par(op)
}
\keyword{datasets}
