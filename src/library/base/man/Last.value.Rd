\name{Last.value}
\alias{.Last.value}
\title{Value of Last Evaluated Expression}
\usage{
.Last.value
}
\description{
Internal evaluation of an \R expression is always assigned to
\code{.Last.value} (in \code{package:base}) before further processing
(e.g. printing).
}
\note{
Do not assign to \code{.Last.value}, because this will always shadow the
one in \code{package:base}.
}
\seealso{\code{\link{eval}}
}
\examples{
gamma(1:15)          # think of some intensive calculation...
fac14 <- .Last.value # keep them

library("eda") # returns invisibly
.Last.value    # shows what library(.) above returned
}
\keyword{programming}
