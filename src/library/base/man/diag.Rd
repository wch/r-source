\name{diag}
\title{Matrix Diagonals}
\usage{
diag(x, nrow, ncol)
diag(x) <- value
}
\alias{diag}
\alias{diag<-}
\description{
  Extract or replace the diagonal of a matrix,
  or construct a diagonal matrix.
}
\value{
  If \code{x} is a matrix then \code{diag(x)} returns the diagonal of
  \code{x}.  The resulting vector will have \code{\link{names}} if the
  matrix \code{x} has matching column and row names.

  If \code{x} is a vector (or 1D array) of length two or more,
  then \code{diag(x)} returns a diagonal matrix whose diagonal is \code{x}.

  If \code{x} is a vector of length one then \code{diag(x)} returns an
  identity matrix of order the nearest integer to \code{x}.  The
  dimension of the returned matrix can be specified by \code{nrow} and
  \code{ncol} (the default is square).

  The assignment form sets the diagonal of the matrix \code{x} to the
  given value(s).
}
\note{
  Using \code{diag(x)} can have unexpected effects if \code{x} is a
  vector that could be of length one. Use \code{diag(x, nrow =
    length(x))} for consistent behaviour. 
}
\seealso{
\code{\link{matrix}}.
}
\examples{
dim(diag(3))
diag(10,3,4) # guess what?
all(diag(1:3) == {m <- matrix(0,3,3); diag(m) <- 1:3; m})

diag(var(M <- cbind(X=1:5, Y=rnorm(5))))#-> vector with names "X" and "Y"
rownames(M) <- c(colnames(M),rep("",3));
M; diag(M) #  named as well

\testonly{
diag(array(1:4, dim=5))
## test behaviour with 0 rows or columns
diag(0)
z <- matrix(0, 0, 4)
diag(z)
diag(z) <- numeric(0)
z
}
}
\keyword{array}
