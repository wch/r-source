% File src/library/base/man/col.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{col}
\title{Column Indexes}
\alias{col}
\alias{.col}
\description{
  Returns a matrix of integers indicating their column number in a
  matrix-like object, or a factor of column labels.
}
\usage{
col(x, as.factor = FALSE)
.col(dim)
}
\arguments{
  \item{x}{a matrix-like object, that is one with a two-dimensional
    \code{dim}.}
  \item{dim}{a matrix dimension, i.e., an integer valued numeric vector of
    length two (with non-negative entries).}
  \item{as.factor}{a logical value indicating whether the value should
    be returned as a factor of column labels (created if necessary)
    rather than as numbers.}
}
\value{
  An integer (or factor) matrix with the same dimensions as \code{x} and whose
  \code{ij}-th element is equal to \code{j} (or the \code{j}-th column label).
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{row}} to get rows;
  \code{\link{slice.index}} for a general way to get slice indices
  in an array.
}
\examples{
# extract an off-diagonal of a matrix
ma <- matrix(1:12, 3, 4)
ma[row(ma) == col(ma) + 1]

# create an identity 5-by-5 matrix more slowly than diag(n = 5):
x <- matrix(0, nrow = 5, ncol = 5)
x[row(x) == col(x)] <- 1

(i34 <- .col(3:4))
stopifnot(identical(i34, .col(c(3,4)))) # 'dim' maybe "double"
}
\keyword{array}
