\name{\%*\%}
\title{Matrix Multiplication}
\usage{
a\%*\%b
}
\alias{\%*\%}
\alias{matmult}
\description{
  Multiplies two matrices, if they are conformable.
  If one argument is a vector, it will be coerced to a either a row or column
  matrix to make the two arguments conformable.
  If both are vectors it will return the inner product.
}
\value{
The matrix product
}
\seealso{\code{\link{matrix}}, \code{\link{Arithmetic}}, \code{\link{diag}}.}
\examples{
x <- 1:4
x\%*\%x # scalar ("inner") product
y <- diag(x)
z <- matrix(1:12,ncol=3,nrow=4)
y\%*\%z
y\%*\%x
x\%*\%z
}
\keyword{array}
\keyword{arith}
