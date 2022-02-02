% File src/library/base/man/zapsmall.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{zapsmall}
\alias{zapsmall}
\title{Rounding of Numbers: Zapping Small Ones to Zero}
\usage{
zapsmall(x, digits = getOption("digits"))
}
\description{
  \code{zapsmall} determines a \code{digits} argument \code{dr} for
  calling \code{round(x, digits = dr)} such that values close to
  zero (compared with the maximal absolute value) are \sQuote{zapped},
  i.e., replaced by \code{0}.
}
\arguments{
  \item{x}{a numeric or complex vector or any \R number-like object
    which has a \code{\link{round}} method and basic arithmetic methods
    including \code{\link{log10}()}.}
  \item{digits}{integer indicating the precision to be used.}
}
\references{
  Chambers, J. M. (1998)
  \emph{Programming with Data.  A Guide to the S Language}.
  Springer.
}
\examples{
x2 <- pi * 100^(-1:3)
print(x2 / 1000, digits = 4)
zapsmall(x2 / 1000, digits = 4)

zapsmall(exp(1i*0:4*pi/2))
}
\keyword{arith}
