% File src/library/base/man/rle.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{rle}
\title{Run Length Encoding}
\alias{rle}
\alias{inverse.rle}
\alias{print.rle}
\concept{runs}
\description{
  Compute the lengths and values of runs of equal values in a vector
  -- or the reverse operation.
}
\usage{
rle(x)
inverse.rle(x, \dots)

\method{print}{rle}(x, digits = getOption("digits"), prefix = "", \dots)
}
\arguments{
  \item{x}{a vector (atomic, not a list) for \code{rle()};
    an object of class \code{"rle"} for \code{inverse.rle()}.}
  \item{\dots}{further arguments; ignored here.}
  \item{digits}{number of significant digits for printing, see
    \code{\link{print.default}}.}
  \item{prefix}{character string, prepended to each printed line.}
}
\details{
  \sQuote{vector} is used in the sense of \code{\link{is.vector}}.

  Missing values are regarded as unequal to the previous value, even if
  that is also missing.

  \code{inverse.rle()} is the inverse function of \code{rle()},
  reconstructing \code{x} from the runs.
}
\value{
  \code{rle()} returns an object of class \code{"rle"} which is a list
  with components:
  \item{lengths}{an integer vector containing the length of each run.}
  \item{values}{a vector of the same length as \code{lengths} with the
    corresponding values.}

  \code{inverse.rle()} returns an atomic vector.
}
\examples{
x <- rev(rep(6:10, 1:5))
rle(x)
## lengths [1:5]  5 4 3 2 1
## values  [1:5] 10 9 8 7 6

z <- c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
rle(z)
rle(as.character(z))
print(rle(z), prefix = "..| ")

N <- integer(0)
stopifnot(x == inverse.rle(rle(x)),
          identical(N, inverse.rle(rle(N))),
          z == inverse.rle(rle(z)))
}
\keyword{manip}
