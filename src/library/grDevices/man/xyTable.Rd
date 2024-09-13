% File src/library/grDevices/man/xyTable.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{xyTable}
\alias{xyTable}
\title{Multiplicities of (x,y) Points, e.g., for a Sunflower Plot}
\concept{2D binning}
\description{
  Given (x,y) points, determine their multiplicity -- checking for
  equality only up to some (crude kind of) noise.  Note that this is
  special kind of 2D binning.
}
\usage{
xyTable(x, y = NULL, digits)
}
\arguments{
  \item{x, y}{numeric vectors of the same length; alternatively other
    (x, y) argument combinations as allowed by
    \code{\link{xy.coords}(x, y)}.}
  \item{digits}{integer specifying the significant digits to be used for
    determining equality of coordinates.  These are compared after
    rounding them via \code{\link{signif}(*, digits)}.}
}
\value{
  A list with three components of same length,
  \item{x}{x coordinates, rounded and sorted.}
  \item{y}{y coordinates, rounded (and sorted within \code{x}).}
  \item{number}{multiplicities (positive integers); i.e.,
    \code{number[i]} is the multiplicity of \code{(x[i], y[i])}.}
}
\note{
  Missing values in the coordinates are counted towards the multiplicities. The
  sum of the multiplicities will equal the number of coordinates.
}
\seealso{\code{\link{sunflowerplot}} which typically uses
  \code{xyTable()}; \code{\link{signif}}.
}
\examples{
xyTable(iris[, 3:4], digits = 6)

## If missing coordinates exist, they are also counted
iris2 <- iris[1:10, 3:4]
iris2[4, 2] <- NA
iris2[c(3, 5), ] <- NA
xyTable(iris2)

## Discretized uncorrelated Gaussian:
\dontshow{set.seed(1)}
xy <- data.frame(x = round(sort(stats::rnorm(100))), y = stats::rnorm(100))
xyTable(xy, digits = 1)
}
\keyword{dplot}
