\name{t}
\title{Matrix Transpose}
\usage{
t(x)
}
\alias{t}
\alias{t.default}
\alias{t.data.frame}
\description{
Given a matrix or \code{\link{data.frame}} \code{x},
\code{t} returns the transpose (matrix or data.frame) of \code{x}.
}
\examples{
a <- matrix(1:30, 5,6)
ta <- t(a) ##-- i.e.,  a[i,j] == ta[j,i] for all i,j :
for(j in seq(ncol(a)))
  if(! a[,j] == ta[j,]) stop("wrong transpose")
}
\keyword{array}
