\name{lapply}
\title{Apply a Function Over a List}
\usage{
lapply(x, fun, \dots)
sapply(x, fun, \dots, simplify = TRUE)
}
\alias{lapply}
\alias{sapply}
\arguments{
  \item{x}{list (or vector for \code{sapply}) to be used.}
  \item{fun}{the function to be applied.
    In the case of functions like \code{+},
    \code{\%*\%}, etc., the function name must be quoted.}
  \item{\dots}{optional arguments to \code{fun}.}
  \item{simplify}{logical; should the result be simplified to a vector
    if possible?}
}
\description{
  \code{lapply} returns a list of the same length as \code{x}.  Each
  element of which is the result of applying \code{fun} to the
  corresponding element of \code{x}.

  \code{sapply} is a ``user-friendly'' version of \code{lapply} also
  accepting vectors as \code{x}, and returning a vector or array with
  \code{dimnames} if appropriate.
}
\seealso{
  \code{\link{apply}}, \code{\link{tapply}}.
}
\examples{
x <- list(a = 1:10, beta = exp(-3:3), logic = c(T,F,F,T))
# compute the list mean for each list element
lapply(x,mean)
# median and quartiles for each list element
lapply(x, quantile, probs = 1:3/4)
sapply(x, quantile)
sapply(sapply(3:9, seq), fivenum)
}
\keyword{iteration}
\keyword{list}
