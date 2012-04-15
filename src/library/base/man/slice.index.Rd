% File src/library/base/man/slice.index.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{slice.index}
\alias{slice.index}
\title{Slice Indexes in an Array}
\description{
  Returns a matrix of integers indicating the number of their slice in a
  given array.
}
\usage{
slice.index(x, MARGIN)
}
\arguments{
  \item{x}{an array.  If \code{x} has no dimension attribute, it is
    considered a one-dimensional array.}
  \item{MARGIN}{an integer giving the dimension number to slice by.}
}
\value{
  An integer array \code{y} with dimensions corresponding to those of
  \code{x} such that all elements of slice number \code{i} with respect
  to dimension \code{MARGIN} have value \code{i}.
}
\seealso{
  \code{\link{row}} and \code{\link{col}} for determining row and column
  indexes; in fact, these are special cases of \code{slice.index}
  corresponding to \code{MARGIN} equal to 1 and 2, respectively when \code{x}
  is a matrix.
}
\examples{
x <- array(1 : 24, c(2, 3, 4))
slice.index(x, 2)
}
\keyword{array}
