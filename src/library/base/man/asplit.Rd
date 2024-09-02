\name{asplit}
\alias{asplit}
\title{Split Array/Matrix By Its Margins}
\description{Split an array or matrix by its margins.}
\usage{
asplit(x, MARGIN, drop = FALSE)
}
\arguments{
  \item{x}{an array, including a matrix.}
  \item{MARGIN}{a vector giving the margins to split by.
    E.g., for a matrix \code{1} indicates rows, \code{2} indicates
    columns, \code{c(1, 2)} indicates rows and columns.
    Where \code{x} has named dimnames, it can be a character vector
    selecting dimension names.}
  \item{drop}{a logical indicating whether the splits should drop
    dimensions and dimnames.}
}
\value{
  A \dQuote{list array} with dimension \eqn{dv} and each element an
  array of dimension \eqn{de} and dimnames preserved as available if
  \code{drop} is false and a vector otherwise, where
  \eqn{dv} and \eqn{de} are, respectively, the dimensions of \code{x}
  included and not included in \code{MARGIN}.
}
\details{
  Since \R 4.1.0, one can also obtain the splits (less efficiently)
  using \code{apply(x, MARGIN, identity, simplify = FALSE)}.
  The values of the splits can also be obtained (less efficiently) by
  \code{split(x, slice.index(x, MARGIN))}.
}
\examples{
## A 3-dimensional array of dimension 2 x 3 x 4:
d <- 2 : 4
x <- array(seq_len(prod(d)), d)
x
## Splitting by margin 2 gives a 1-d list array of length 3
## consisting of 2 x 4 arrays:
asplit(x, 2)
## Splitting by margins 1 and 2 gives a 2 x 3 list array
## consisting of 1-d arrays of length 4:
asplit(x, c(1, 2))
## Compare to
split(x, slice.index(x, c(1, 2)))

## A 2 x 3 matrix:
(x <- matrix(1 : 6, 2, 3))
## To split x by its rows, one can use
asplit(x, 1)
## or less efficiently
split(x, slice.index(x, 1))
split(x, row(x))
}
\keyword{array}
