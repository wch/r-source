\name{mat.or.vec}
\title{Create a Matrix or a Vector}
\usage{
mat.or.vec(nr, nc)
}
\alias{mat.or.vec}
\description{
  \code{mat.or.vec} creates an \code{nr} by \code{nc} zero matrix if
  \code{nc} is greater than 1, and a zero vector of length \code{nr} if
  \code{nc} equals 1.
}
\examples{
mat.or.vec(3, 1)
mat.or.vec(3, 2)
}
\keyword{array}
