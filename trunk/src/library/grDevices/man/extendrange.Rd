% File src/library/grDevices/man/extendrange.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{extendrange}
\alias{extendrange}
\title{Extend a Numerical Range by a Small Percentage}
\description{
  Extends a numerical range by a small percentage, i.e., fraction,
  \emph{on both sides}.
}
\usage{
extendrange(x, r = range(x, na.rm = TRUE), f = 0.05)
}
\arguments{
  \item{x}{numeric vector; not used if \code{r} is specified.}
  \item{r}{numeric vector of length 2; defaults to the
    \code{\link{range}} of \code{x}.}
  \item{f}{positive number(s) specifying the fraction by which the range should be
    extended.  If longer than one, \code{f[1]} is used on the left, and
    \code{f[2]} on the right.}
}
\seealso{\code{\link{range}}; \code{\link{pretty}} which can be
  considered a sophisticated extension of \code{extendrange}.
}
\value{
  A numeric vector of length 2, \code{r + c(-f1,f2) * diff(r)}, where
  f1 is \code{f[1]} and f2 is \code{f[2]} or \code{f} if it is of length
  one.
}
\examples{
x <- 1:5
(r <- range(x))         # 1    5
extendrange(x)          # 0.8  5.2
extendrange(x, f= 0.01) # 0.96 5.04

## extend more to the right:
extendrange(x, f=c(.01,.03)) # 0.96 5.12

## Use 'r' if you have it already:
stopifnot(identical(extendrange(r = r),
                    extendrange(x)))
}
\keyword{dplot}
