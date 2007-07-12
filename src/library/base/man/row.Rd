\name{row}
\alias{row}
\title{Row Indexes}
\description{
  Returns a matrix of integers indicating their row number in a
  matrix-like object.
}
\usage{
row(x, as.factor = FALSE)
}
\arguments{
  \item{x}{a matrix-like object, that is one with a two-dimensional
    \code{dim}.} 
  \item{as.factor}{a logical value indicating whether the value should
    be returned as a factor rather than as numeric.}
}
\value{
  An integer matrix with the same dimensions as \code{x} and whose
  \code{ij}-th element is equal to \code{i}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{col}} to get columns.
}
\examples{
x <- matrix(1:12, 3, 4)
# extract the diagonal of a matrix
dx <- x[row(x) == col(x)]
dx

# create an identity 5-by-5 matrix
x <- matrix(0, nrow = 5, ncol = 5)
x[row(x) == col(x)] <- 1
x
}
\keyword{array}
