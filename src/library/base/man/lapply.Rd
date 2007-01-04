\name{lapply}
\alias{lapply}
\alias{sapply}
\alias{replicate}
\title{Apply a Function over a List or Vector}
\usage{
lapply(X, FUN, \dots)

sapply(X, FUN, \dots, simplify = TRUE, USE.NAMES = TRUE)

replicate(n, expr, simplify = TRUE)
}
\arguments{
  \item{X}{a vector (atomic or list) or an expressions vector.  Other
    objects (including classed objects) will be coerced by
    \code{\link{as.list}}.}
  \item{FUN}{the function to be applied to each element of \code{X}:
    see Details.  In the case of functions like \code{+}, \code{\%*\%}, etc.,
    the function name must be backquoted or quoted.}
  \item{\dots}{optional arguments to \code{FUN}.}
  \item{simplify}{logical; should the result be simplified to a vector
    or matrix if possible?}
  \item{USE.NAMES}{logical; if \code{TRUE} and if \code{X} is character,
    use \code{X} as \code{\link{names}} for the result unless it had names
    already.}
  \item{n}{number of replications.}
  \item{expr}{expression (language object, usually a call)
    to evaluate repeatedly.}
}
\description{
  \code{lapply} returns a list of the same length as \code{X}, each
  element of which is the result of applying \code{FUN} to the
  corresponding element of \code{X}.

  \code{sapply} is a \dQuote{user-friendly} version of \code{lapply}
  by default returning a vector or matrix if appropriate.

  \code{replicate} is a wrapper for the common use of \code{sapply} for
  repeated evaluation of an expression (which will usually involve
  random number generation).
}
\details{
  \code{FUN} is found by a call to \code{\link{match.fun}} and typically
  is specified as a function or a symbol (e.g. a backquoted name) or a
  character string specifying a function to be searched for from the
  environment of the call to \code{lapply}.

  Function \code{FUN} must be able to accept as input any of the
  elements of \code{X}.  If the latter is an atomic vector, \code{FUN}
  will always be passed a length-one vector of the same type as \code{X}.

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
  \code{\link{apply}}, \code{\link{tapply}},
  \code{\link{mapply}} for applying a function to \bold{m}ultiple
  arguments, and \code{\link{rapply}} for a \bold{r}ecursive version of
  \code{lapply()}, \code{\link{eapply}} for applying a function to each
  entry in an \code{\link{environment}}.
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
