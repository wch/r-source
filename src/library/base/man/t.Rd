\name{t}
\alias{t}
\alias{t.default}
\alias{t.data.frame}
\title{Matrix Transpose}
\description{
Given a matrix or \code{\link{data.frame}} \code{x},
\code{t} returns the transpose (matrix or data.frame) of \code{x}.
}
\usage{
t(x)
}
\arguments{
  \item{x}{a matrix or data frame.}
}
\seealso{
  \code{\link{aperm}} for permuting the dimensions of arrays.
}
\examples{
a <- matrix(1:30, 5,6)
ta <- t(a) ##-- i.e.,  a[i, j] == ta[j, i] for all i,j :
for(j in seq(ncol(a)))
  if(! a[, j] == ta[j, ]) stop("wrong transpose")
}
\keyword{array}
