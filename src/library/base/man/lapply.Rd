\name{lapply}
\title{Apply a Function over a List or Vector}
\usage{
lapply(X, FUN, \dots)
sapply(X, FUN, \dots, simplify = TRUE, USE.NAMES = TRUE)

replicate(n, expr, simplify = TRUE) 
}
\alias{lapply}
\alias{sapply}
\alias{replicate}
\arguments{
  \item{X}{list or vector to be used.}
  \item{FUN}{the function to be applied.
    In the case of functions like \code{+},
    \code{\%*\%}, etc., the function name must be quoted.}
  \item{\dots}{optional arguments to \code{FUN}.}
  \item{simplify}{logical; should the result be simplified to a vector
    if possible?}
  \item{USE.NAMES}{logical; if \code{TRUE} and if \code{X} is character,
    use \code{X} as \code{\link{names}} for the result unless it had names
    already.}
  \item{n}{Number of replications.}
  \item{expr}{Expression to evaluate repeatedly.}
}
\description{
  \code{lapply} returns a list of the same length as \code{X}.  Each
  element of which is the result of applying \code{FUN} to the
  corresponding element of \code{X}.

  \code{sapply} is a \dQuote{user-friendly} version of \code{lapply}
  also accepting vectors as \code{X}, and returning a vector or matrix
  with \code{dimnames} if appropriate.

  \code{replicate} is a wrapper for the common use of \code{sapply} for
  repeated evaluation of an expression (which will usually involve
  random number generation.)
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{apply}}, \code{\link{tapply}}.
}
\examples{
x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))
# compute the list mean for each list element
lapply(x,mean)
# median and quartiles for each list element
lapply(x, quantile, probs = 1:3/4)
sapply(x, quantile)
str(i39 <- sapply(3:9, seq))# list of vectors
sapply(i39, fivenum)
hist(replicate(100, mean(rexp(10))))
}
\keyword{iteration}
\keyword{list}
