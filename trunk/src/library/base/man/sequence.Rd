% File src/library/base/man/sequence.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{sequence}
\title{Create A Vector of Sequences}
\usage{
sequence(nvec, \dots)
\method{sequence}{default}(nvec, from = 1L, by = 1L, \dots)
}
\alias{sequence}
\alias{sequence.default}
\arguments{
  \item{nvec}{coerced to a non-negative integer vector each element of which
    specifies the length of a sequence.}
  \item{from}{coerced to an integer vector each element of which
    specifies the first element of a sequence.}
  \item{by}{coerced to an integer vector each element of which
    specifies the step size between elements of a sequence.}
  \item{\dots}{additional arguments passed to methods.}
}
\description{
  The default method for \code{sequence} generates the sequence
  \code{\link{seq}(from[i], by = by[i], length.out = nvec[i])} for each
  element \code{i} in the parallel (and recycled) vectors \code{from},
  \code{by} and \code{nvec}. It then returns the result of concatenating
  those sequences.
}
\details{
  Negative values are supported for \code{from} and
  \code{by}. \code{sequence(nvec, from, by=0L)} is equivalent to
  \code{rep(from, each=nvec)}.

  This function was originally implemented in R with fewer features, but
  it has since become more flexible, and the default method is
  implemented in C for speed.
}
\seealso{
  \code{\link{gl}}, \code{\link{seq}}, \code{\link{rep}}.
}
\author{Of the current version, Michael Lawrence based on code from the
  S4Vectors Bioconductor package}
\examples{
sequence(c(3, 2)) # the concatenated sequences 1:3 and 1:2.
#> [1] 1 2 3 1 2
sequence(c(3, 2), from=2L)
#> [1] 2 3 4 2 3
sequence(c(3, 2), from=2L, by=2L)
#> [1] 2 4 6 2 4
sequence(c(3, 2), by=c(-1L, 1L))
#> [1] 1 0 -1 1 2
}
\keyword{manip}
