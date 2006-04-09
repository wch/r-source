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
  \item{X}{list or (atomic) vector to be used.}
  \item{FUN}{the function to be applied to each element of \code{X}.
    In the case of functions like \code{+}, \code{\%*\%}, etc.,
    the function name must be quoted.}
  \item{\dots}{optional arguments to \code{FUN}.}
  \item{simplify}{logical; should the result be simplified to a vector
    or matrix if possible?}
  \item{USE.NAMES}{logical; if \code{TRUE} and if \code{X} is character,
    use \code{X} as \code{\link{names}} for the result unless it had names
    already.}
  \item{n}{number of replications.}
  \item{expr}{expression to evaluate repeatedly.}
}
\description{
  \code{lapply} returns a list of the same length as \code{X}.  Each
  element of which is the result of applying \code{FUN} to the
  corresponding element of \code{X}.

  \code{sapply} is a \dQuote{user-friendly} version of \code{lapply}
  by default returning a vector or matrix if appropriate.

  \code{replicate} is a wrapper for the common use of \code{sapply} for
  repeated evaluation of an expression (which will usually involve
  random number generation).
}
\details{
  Function \code{FUN} must be able to accept as input any of the
  elements of \code{X}.  If the latter is an atomic vector, \code{FUN}
  will always be passed a length-one vector.
  
  Simplification in \code{sapply} is only attempted if \code{X} has
  length greater than zero and if the return values from all elements
  of \code{X} are all of the same (positive) length.  If the common
  length is one the result is a vector, and if greater than one is a
  matrix with a column corresponding to each element of \code{X}.

  The mode of the simplified answer is chosen to accommodate the modes of
  all the values returned by the calls to \code{FUN}: see \code{\link{unlist}}.

  if \code{X} has length 0, the return value of \code{sapply} is always
  a 0-length list.
}
\note{
  \code{sapply(*, simplify = FALSE, USE.NAMES = FALSE)} is
  equivalent to \code{lapply(*)}.
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
i39 <- sapply(3:9, seq) # list of vectors
sapply(i39, fivenum)

hist(replicate(100, mean(rexp(10))))
}
\keyword{iteration}
\keyword{list}
