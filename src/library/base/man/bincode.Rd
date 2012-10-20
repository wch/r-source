% File src/library/base/man/bincode.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{.bincode}
\alias{.bincode}
\title{Bin a Numeric Vector}
\description{
  Bin a numeric vector and return integer codes for the binning.
}
\usage{
.bincode(x, breaks, right = TRUE, include.lowest = FALSE)
}
\arguments{
  \item{x}{a numeric vector which is to be converted to integer codes by
    binning.}
  \item{breaks}{a numeric vector of two or more cut points, sorted in
    increasing order.}
  \item{right}{logical, indicating if the intervals should be closed on
    the right (and open on the left) or vice versa.}
  \item{include.lowest}{logical, indicating if an \sQuote{x[i]} equal to
    the lowest (or highest, for \code{right = FALSE}) \sQuote{breaks}
    value should be included in the first (or last) bin.}
}
\details{
  This is a \sQuote{barebones} version of \code{cut.default(labels =
    FALSE)} intended for use in other functions which have checked the
  arguments passed.  (Note the different order of the arguments they have
  in common.)

  Unlike \code{\link{cut}}, the \code{breaks} do not need to be unique.
  An input can only fall into a zero-length interval if it is closed
  at both ends, so only if \code{include.lowest = TRUE} and it is the
  first (or last for \code{right = FALSE}) interval.
}
\value{
  An integer vector of the same length as \code{x} indicating which bin
  each element falls into (the leftmost bin being bin \code{1}).
  \code{NaN} and \code{NA} elements of \code{x} are mapped to
  \code{NA} codes, as are values outside range of \code{breaks}.
}
\seealso{
  \code{\link{cut}}, \code{\link{tabulate}}
}
\examples{
## An example with non-unique breaks:
x <- c(0, 0.01, 0.5, 0.99, 1)
b <- c(0, 0, 1, 1)
.bincode(x, b, TRUE)
.bincode(x, b, FALSE)
.bincode(x, b, TRUE, TRUE)
.bincode(x, b, FALSE, TRUE)
}

\keyword{category}
