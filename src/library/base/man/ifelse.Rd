\name{ifelse}
\title{Conditional Element Selection}
\usage{
ifelse(test, yes, no)
}
\alias{ifelse}
\description{
\code{ifelse} returns a value with the same shape as
\code{test} which is filled with elements selected
from either \code{yes} or \code{no}
depending on whether the element of \code{test}
is \code{TRUE} or \code{FALSE}.
If \code{yes} or \code{no} are too short, their elements are recycled.
}
\arguments{
  \item{test}{a logical vector}
  \item{yes}{return values for true elements of \code{test}.}
  \item{no}{return values for false elements of \code{test}.}
}
\seealso{
\code{\link{if}}.
}
\examples{
x <- c(6:-4)
sqrt(x)#- gives warning
sqrt(ifelse(x >= 0, x, NA))# no warning

## Note: the following also gives the warning !
ifelse(x >= 0, sqrt(x), NA)
}
\keyword{logic}
\keyword{programming}
