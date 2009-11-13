% File src/library/base/man/lapply.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
% Distributed under GPL 2 or later

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
    see \sQuote{Details}.  In the case of functions like
    \code{+}, \code{\%*\%}, etc.,
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

  \code{sapply} is a user-friendly version of \code{lapply}
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

  Users of S4 classes should pass a list to \code{lapply}: the internal
  coercion is done by the system \code{as.list} in the base namespace
  and not one defined by a user (e.g. by setting S4 methods on the
  system function).
}
\value{
  For \code{lapply} and \code{sapply(simplify=FALSE)}, a list.

  For \code{sapply(simplify=TRUE)} and \code{replicate}: if \code{X} has
  length zero or \code{n = 0}, an empty list.  Otherwise an atomic
  vector or matrix or list of the same length as \code{X} (of length
  \code{n} for \code{replicate}).  If simplification occurs, the output
  type is determined from the highest type of the return values in the
  hierarchy NULL < raw < logical < integer < real < complex < character
  < list < expression, after coercion of pairlists to lists.
}
\note{
  \code{sapply(*, simplify = FALSE, USE.NAMES = FALSE)} is
  equivalent to \code{lapply(*)}.

  For historical reasons, the calls created by \code{lapply} are
  unevaluated, and code has been written (e.g. \code{bquote}) that
  relies on this.  This means that the recorded call is always of the
  form \code{FUN(X[[0L]], ...)}, with \code{0L} replaced by the current
  integer index.  This not normally a problem, but it can be if
  \code{FUN} uses \code{\link{sys.call}} or \code{\link{match.call}} or
  if it is a primitive function that makes use of the call.  This means
  that it is often safer to call primitive functions with a wrapper, so
  that e.g. \code{lapply(ll, function(x) is.numeric(x))} is required in
  \R 2.7.1 to ensure that method dispatch for \code{is.numeric} occurs
  correctly.
  
  If \code{expr} is a function call, be aware of assumptions about where
  it is evaluated, and in particular what \code{\dots} might refer to.
  You can pass additional named arguments to a function call as
  additional named arguments to \code{replicate}: see \sQuote{Examples}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{apply}}, \code{\link{tapply}},
  \code{\link{mapply}} for applying a function to \bold{m}ultiple
  arguments, and \code{\link{rapply}} for a \bold{r}ecursive version of
  \code{lapply()}, \code{\link{eapply}} for applying a function to each
  entry in an \code{\link{environment}}.
}
\examples{
require(stats); require(graphics)

x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))
# compute the list mean for each list element
lapply(x,mean)
# median and quartiles for each list element
lapply(x, quantile, probs = 1:3/4)
sapply(x, quantile)
i39 <- sapply(3:9, seq) # list of vectors
sapply(i39, fivenum)

hist(replicate(100, mean(rexp(10))))

## use of replicate() with parameters:
foo <- function(x=1, y=2) c(x,y)
# does not work: bar <- function(n, ...) replicate(n, foo(...))
bar <- function(n, x) replicate(n, foo(x=x))
bar(5, x=3)
}
% taken from PR#8472
\keyword{iteration}
\keyword{list}
