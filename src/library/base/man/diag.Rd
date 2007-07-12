\name{diag}
\title{Matrix Diagonals}
\usage{
diag(x = 1, nrow, ncol = n)
diag(x) <- value
}
\alias{diag}
\alias{diag<-}
\description{
  Extract or replace the diagonal of a matrix,
  or construct a diagonal matrix.
}
\arguments{
  \item{x}{a matrix, vector or 1D array.}
  \item{nrow, ncol}{Optional dimensions for the result.}
  \item{value}{either a single value or a vector of length equal to that
    of the current diagonal.  Should be of a mode which can be coerced
    to that of \code{x}.}
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

  The replacement form sets the diagonal of the matrix \code{x} to the
  given value(s).
}
\note{
  Using \code{diag(x)} can have unexpected effects if \code{x} is a
  vector that could be of length one. Use \code{diag(x, nrow =
    length(x))} for consistent behaviour.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
\code{\link{upper.tri}}, \code{\link{lower.tri}}, \code{\link{matrix}}.
}
\examples{
require(stats)
dim(diag(3))
diag(10,3,4) # guess what?
all(diag(1:3) == {m <- matrix(0,3,3); diag(m) <- 1:3; m})

diag(var(M <- cbind(X=1:5, Y=stats::rnorm(5))))#-> vector with names "X" and "Y"
rownames(M) <- c(colnames(M),rep("",3));
M; diag(M) #  named as well
}
\keyword{array}
