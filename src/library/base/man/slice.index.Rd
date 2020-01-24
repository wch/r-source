% File src/library/base/man/slice.index.Rd
% Part of the R package, https://www.R-project.org
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
  \item{MARGIN}{an integer vector giving the dimension numbers to slice 
    by.}
}
\value{
  An integer array \code{y} with dimensions corresponding to those of
  \code{x}.
}
\details{
  If \code{MARGIN} gives a single dimension, then all elements of slice
  number \code{i} with respect to this have value \code{i}.  In general,
  slice numbers are obtained by numbering all combinations of indices in
  the dimensions given by \code{MARGIN} in column-major order.  I.e.,
  with \eqn{m_1}, \ldots, \eqn{m_k} the dimension numbers (elements of
  \code{MARGIN}) sliced by and \eqn{d_{m_1}}, \ldots, \eqn{d_{m_k}} the
  corresponding extents, and \eqn{n_1 = 1}, \eqn{n_2 = d_{m_1}}, \ldots,
  \eqn{n_k = d_{m_1} \cdots d_{m_{k-1}}}{n_k = d_{m_1} \dots d_{m_{k-1}}},
  the number of the slice where dimension \eqn{m_1} has value \eqn{i_1},
  \ldots, dimension \eqn{m_k} has value \eqn{i_k} is 
  \eqn{1 + n_1 (i_1 - 1) + \cdots + n_k (i_k - 1)}{1 + n_1 (i_1 - 1) +
    \dots + n_k (i_k - 1)}.
}
\seealso{
  \code{\link{row}} and \code{\link{col}} for determining row and column
  indexes; in fact, these are special cases of \code{slice.index}
  corresponding to \code{MARGIN} equal to 1 and 2, respectively when
  \code{x} is a matrix.
}
\examples{
x <- array(1 : 24, c(2, 3, 4))
slice.index(x, 2)
slice.index(x, c(1, 3))
## When slicing by dimensions 1 and 3, slice index 5 is obtained for
## dimension 1 has value 1 and dimension 3 has value 3 (see above):
which(slice.index(x, c(1, 3)) == 5, arr.ind = TRUE)
}
\keyword{array}
