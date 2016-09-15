% File src/library/base/man/tabulate.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{tabulate}
\title{Tabulation for Vectors}
\description{
  \code{tabulate} takes the integer-valued vector \code{bin} and counts
  the number of times each integer occurs in it.
}
\usage{
tabulate(bin, nbins = max(1, bin, na.rm = TRUE))
}
\alias{tabulate}
\arguments{
    \item{bin}{a numeric vector (of positive integers), or a factor.
      \link{Long vectors} are supported.}
    \item{nbins}{the number of bins to be used.}
}
\details{
  \code{tabulate} is the workhorse for the \code{\link{table}} function.

  If \code{bin} is a factor, its internal integer representation
  is tabulated.

  If the elements of \code{bin} are numeric but not integers,
  they are truncated by \code{\link{as.integer}}.
}
\value{
  An integer valued \code{\link{integer}} or \code{\link{double}} vector
  (without names).  There is a bin for each of the values \code{1,
  \dots, nbins}; values outside that range and \code{NA}s are (silently)
  ignored.

  On 64-bit platforms \code{bin} can have \eqn{2^{31}}{2^31} or more
  elements (i.e., \code{length(bin) > .Machine$integer.max}), and hence
  a count could exceed the maximum integer.  For this reason, the return
  value is of type double for such long \code{bin} vectors.
}
\seealso{
  \code{\link{table}}, \code{\link{factor}}.
}
\examples{
tabulate(c(2,3,5))
tabulate(c(2,3,3,5), nbins = 10)
tabulate(c(-2,0,2,3,3,5))  # -2 and 0 are ignored
tabulate(c(-2,0,2,3,3,5), nbins = 3)
tabulate(factor(letters[1:10]))
}
\keyword{arith}
