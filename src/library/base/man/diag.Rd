\name{diag}
\title{Matrix Diagonals}
\usage{
diag(x, nrow, ncol)
diag(x) <- value
}
\alias{diag}
\alias{diag<-}
\value{
If \code{x} is a matrix then \code{diag(x)} returns the diagonal of
\code{x}.  If \code{x} is a vector then \code{x} returns a diagonal
matrix whose diagonal is \code{x}.  If \code{x} is an integer then
\code{diag(x)} returns an identity matrix of order \code{x}.
The dimension of the returned matrix can be specified by
\code{nrow} and \code{ncol} (the default is square).

The assignment form sets the diagonal of the matrix \code{x} to the
given value(s).
}
\seealso{
\code{\link{matrix}}.
}
\examples{
dim(diag(3))
diag(10,3,4) # guess what?
all(diag(1:3) == {m <- matrix(0,3,3); diag(m) <- 1:3; m})
\testonly{
## test behaviour with 0 row or columns
diag(0)
z <- matrix(0, 0, 4)
diag(z)
diag(z) <- numeric(0)
z
}}
\keyword{array}
