\name{is.recursive}
\title{Is an Object Atomic or Recursive?}
\usage{
is.atomic(x)
is.recursive(x)
}
\alias{is.atomic}
\alias{is.recursive}
\description{
\code{is.atomic} returns \code{TRUE} if \code{x}
does not have a list structure and \code{FALSE} otherwise.

\code{is.recursive} returns \code{TRUE} if \code{x} has a recursive
(list-like) structure and \code{FALSE} otherwise.
}
\seealso{\code{\link{is.list}},\code{\link{is.language}}, etc and the
  \code{demo("is.things")}.}
\examples{
is.a.r <- function(x) c(is.atomic(x), is.recursive(x))

is.a.r(c(a=1,b=3))# TRUE FALSE
is.a.r(list())    # FALSE TRUE ??
is.a.r(list(2))   # FALSE TRUE
is.a.r(lm)        #   "
is.a.r(y ~ x)     #   "
is.a.r(expression(x+1))# should be F-T (not in 0.62.3!)
}
\keyword{programming}
\keyword{classes}
