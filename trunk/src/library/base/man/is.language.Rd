\name{is.language}
\alias{is.language}
\title{Is an Object a Language Object?}
\usage{
is.language(x)
}
\description{
  \code{is.language} returns \code{TRUE} if \code{x} is a
  variable \code{\link{name}}, a \code{\link{call}}, or an
  \code{\link{expression}}.
}
\arguments{
  \item{x}{object to be tested.}
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\examples{
ll <- list(a = expression(x^2 - 2*x + 1), b = as.name("Jim"),
           c = as.expression(exp(1)), d = call("sin", pi))
sapply(ll, typeof)
sapply(ll, mode)
stopifnot(sapply(ll, is.language))
}
\keyword{programming}
