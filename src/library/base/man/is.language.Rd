\name{is.language}
\title{Is an Object a Language Object?}
\usage{is.language(x)}
\alias{is.language}
\description{
  \code{is.language} returns \code{TRUE} if \code{x} is either a
  variable \code{\link{name}}, a \code{\link{call}}, or an
  \code{\link{expression}}.
}
\examples{
ll <- list(a = expression(x^2 - 2*x + 1), b = as.name("Jim"),
           c = as.expression(exp(1)), d = call("sin", pi))
sapply(ll, typeof)
sapply(ll, mode)
sapply(ll, is.language)# all TRUE
}
\keyword{programming}
