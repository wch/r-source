\name{Round}
\title{Rounding of Numbers}
\usage{
ceiling(x)
floor(x)
round(x, digits = 0)
signif(x, digits)
trunc(x)
}
\alias{ceiling}
\alias{floor}
\alias{round}
\alias{signif}
\alias{trunc}
\description{
  \code{ceiling} takes a single numeric argument \code{x} and returns a
  numeric vector containing the smallest integers not less than the
  corresponding elements of \code{x}.

  \code{floor} takes a single numeric argument \code{x} and returns a
  numeric vector containing the largest integers not greater than the
  corresponding elements of \code{x}. 

  \code{round} rounds the values in its first argument to the specified
  number of decimal places (default 0).

  \code{signif} rounds the values in its first argument to the specified
  number of significant digits.

  \code{trunc} takes a single numeric argument \code{x} and returns a
  numeric vector containing the integers by truncating the values in
  \code{x} toward \code{0}. 
}
\seealso{
  \code{\link{as.integer}}.
}
\examples{
print(x1 <- seq(-2, 4, by = .5))
x1[trunc(x1) != floor(x1)]
x1[round(x1) != floor(x1 + .5)]
all(trunc(x1) == as.integer(x1))
# TRUE
non.int <- ceiling(x1) != floor(x1)
all(non.int == (ceiling(x1) != trunc(x1) | trunc(x1) != floor(x1)))
# TRUE
all((signif(x1, 1) != round(x1)) == (non.int & abs(x1)<1))
# TRUE

x2 <- pi * 100^(-1:3)
round(x2, 3)
signif(x2, 3)
}
