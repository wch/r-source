\name{is.recursive}
\title{Is an Object Recursive?}
\usage{is.recursive(x)}
\alias{is.recursive}
\description{
\code{is.recursive} returns \code{TRUE} if \code{x} has a recursive
(list-like) structure and \code{FALSE} otherwise.
}
\seealso{\code{\link{is.list}},\code{\link{is.language}}, etc.}
\examples{
is.recursive(c(a=1,b=3))# F
is.recursive(list())    # TRUE ??
is.recursive(list(2))   # TRUE
is.recursive(lm)        #   "
is.recursive(y ~ x)     #   "
is.recursive(expression(x+1))# should be TRUE (not in 0.62.3!)
}
\keyword{programming}
