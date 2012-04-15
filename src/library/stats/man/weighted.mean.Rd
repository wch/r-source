% File src/library/stats/man/weighted.mean.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{weighted.mean}
\alias{weighted.mean}
\alias{weighted.mean.default}
\title{Weighted Arithmetic Mean}
\description{
  Compute a weighted mean.
}
\usage{
weighted.mean(x, w, \dots)

\method{weighted.mean}{default}(x, w, \dots, na.rm = FALSE)
}
\arguments{
  \item{x}{an object containing the values whose weighted mean is to be
    computed.}
  \item{w}{a numerical vector of weights the same length as \code{x} giving
    the weights to use for elements of \code{x}.}
  \item{\dots}{arguments to be passed to or from methods.}
  \item{na.rm}{a logical value indicating whether \code{NA}
    values in \code{x} should be stripped before the computation proceeds.}
}
\details{
  This is a generic function and methods can be defined for the first
  argument \code{x}: apart from the default methods there are methods
  for the date-time classes \code{"POSIXct"}, \code{"POSIXlt"},
  \code{"difftime"} and \code{"Date"}.  The default method will work for
  any numeric-like object for which \code{[}, multiplication, division
  and \code{\link{sum}} have suitable methods, including complex vectors.

  If \code{w} is missing then all elements of \code{x} are given the
  same weight, otherwise the weights coerced to numeric by
  \code{\link{as.numeric}} and normalized to sum to one (if possible: if
  their sum is zero or infinite the value is likely to be \code{NaN}).

  Missing values in \code{w} are not handled specially and so give a
  missing value as the result.  However, as from \R 2.11.0 zero weights
  \emph{are} handled specially and the corresponding \code{x} values are
  omitted from the sum.
}
\value{
  For the default method, a length-one numeric vector.
}
\seealso{
  \code{\link{mean}}
}
\examples{
## GPA from Siegel 1994
wt <- c(5,  5,  4,  1)/15
x <- c(3.7,3.3,3.5,2.8)
xm <- weighted.mean(x, wt)
}
\keyword{univar}
