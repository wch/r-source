\name{t}
\alias{t}
\alias{t.default}
\alias{t.data.frame}
\alias{t.ts}
\title{Matrix Transpose}
\description{
  Given a matrix or \code{\link{data.frame}} \code{x},
  \code{t} returns the transpose of \code{x}.
}
\usage{
t(x)
}
\arguments{
  \item{x}{a matrix or data frame, typically.}
}
\details{
  A data frame is first coerced to a matrix: see \code{\link{as.matrix}}.
  When \code{x} is a vector, it is treated as ``column'', i.e., the
  result is a 1-row matrix.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{aperm}} for permuting the dimensions of arrays.
}
\examples{
a <- matrix(1:30, 5,6)
ta <- t(a) ##-- i.e.,  a[i, j] == ta[j, i] for all i,j :
for(j in seq(ncol(a)))
  if(! all(a[, j] == ta[j, ])) stop("wrong transpose")
}
\keyword{array}
