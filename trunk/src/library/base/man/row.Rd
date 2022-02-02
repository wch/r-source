% File src/library/base/man/row.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{row}
\title{Row Indexes}
\alias{row}
\alias{.row}
\description{
  Returns a matrix of integers indicating their row number in a
  matrix-like object, or a factor indicating the row labels.
}
\usage{
row(x, as.factor = FALSE)
.row(dim)
}
\arguments{
  \item{x}{a matrix-like object, that is one with a two-dimensional
    \code{dim}.}
  \item{dim}{a matrix dimension, i.e., an integer valued numeric vector of
    length two (with non-negative entries).}
  \item{as.factor}{a logical value indicating whether the value should
    be returned as a factor of row labels (created if necessary)
    rather than as numbers.}
}
\value{
  An integer (or factor) matrix with the same dimensions as \code{x} and whose
  \code{ij}-th element is equal to \code{i} (or the \code{i}-th row label).
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{col}} to get columns;
  \code{\link{slice.index}} for a general way to get slice indices
  in an array.
}
\examples{
x <- matrix(1:12, 3, 4)
# extract the diagonal of a matrix - more slowly than diag(x)
dx <- x[row(x) == col(x)]
dx

# create an identity 5-by-5 matrix more slowly than diag(n = 5):
x <- matrix(0, nrow = 5, ncol = 5)
x[row(x) == col(x)] <- 1
x

(i34 <- .row(3:4))
stopifnot(identical(i34, .row(c(3,4)))) # 'dim' maybe "double"
}
\keyword{array}
