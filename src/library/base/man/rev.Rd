\name{rev}
\alias{rev}
\title{Reverse a Vector's Elements}
\usage{
rev(x)
}
\arguments{
  \item{x}{a vector.}
}
\description{
  \code{rev}
  provides a reversed version of its argument.
  It can be used in combination with
  \code{sort} to obtain vectors sorted into descending order.
}
\seealso{\code{\link{seq}}, \code{\link{sort}}.}
\examples{
x <- c(1:5,5:3)
# sort into descending order
rev(sort(x))
stopifnot(rev(1:7) == 7:1)#- don't need `rev' here
}
\keyword{manip}
