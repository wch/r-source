\name{lapply}
\title{Apply a Function Over a List or Vector}
\usage{
lapply(X, FUN, \dots)
sapply(X, FUN, \dots, simplify = TRUE)
}
\alias{lapply}
\alias{sapply}
\arguments{
  \item{X}{list or vector to be used.}
  \item{FUN}{the function to be applied.
    In the case of functions like \code{+},
    \code{\%*\%}, etc., the function name must be quoted.}
  \item{\dots}{optional arguments to \code{FUN}.}
  \item{simplify}{logical; should the result be simplified to a vector
    if possible?}
}
\description{
  \code{lapply} returns a list of the same length as \code{X}.  Each
  element of which is the result of applying \code{FUN} to the
  corresponding element of \code{X}.

  \code{sapply} is a ``user-friendly'' version of \code{lapply} also
  accepting vectors as \code{X}, and returning a vector or array with
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
str(i39 <- sapply(3:9, seq))# list of vectors
sapply(i39, fivenum)
}
\keyword{iteration}
\keyword{list}
