% File src/library/base/man/lapply.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{lapply}
\title{Apply a Function over a List or Vector}
\alias{lapply}
\alias{sapply}
\alias{vapply}
\alias{replicate}
\alias{simplify2array}
\usage{
lapply(X, FUN, \dots)

sapply(X, FUN, \dots, simplify = TRUE, USE.NAMES = TRUE)

vapply(X, FUN, FUN.VALUE, \dots, USE.NAMES = TRUE)

replicate(n, expr, simplify = "array")

simplify2array(x, higher = TRUE)
}
\arguments{
  \item{X}{a vector (atomic or list) or an \code{\link{expression}}
    object.  Other objects (including classed objects) will be coerced
    by \code{base::\link{as.list}}.}
  \item{FUN}{the function to be applied to each element of \code{X}:
    see \sQuote{Details}.  In the case of functions like
    \code{+}, \code{\%*\%}, the function name must be backquoted or quoted.}
  \item{\dots}{optional arguments to \code{FUN}.}
  \item{simplify}{logical or character string; should the result be
    simplified to a vector, matrix or higher dimensional array if
    possible?  For \code{sapply} it must be named and not abbreviated.
    The default value, \code{TRUE}, returns a vector or matrix if appropriate,
    whereas if \code{simplify = "array"} the result may be an
    \code{\link{array}} of \dQuote{rank}
    (\eqn{=}\code{length(dim(.))}) one higher than the result
    of \code{FUN(X[[i]])}.}
  \item{USE.NAMES}{logical; if \code{TRUE} and if \code{X} is character,
    use \code{X} as \code{\link{names}} for the result unless it had names
    already.  Since this argument follows \code{\dots} its name cannot
    be abbreviated.}
  \item{FUN.VALUE}{a (generalized) vector; a template for the return
    value from FUN.  See \sQuote{Details}.}
  \item{n}{integer: the number of replications.}
  \item{expr}{the expression (language object, usually a call)
    to evaluate repeatedly.}

  \item{x}{a list, typically returned from \code{lapply()}.}
  \item{higher}{logical; if true, \code{simplify2array()} will produce a
    (\dQuote{higher rank}) array when appropriate, whereas
    \code{higher = FALSE} would return a matrix (or vector) only.
    These two cases correspond to \code{sapply(*, simplify = "array")} or
    \code{simplify = TRUE}, respectively.}
}
\description{
  \code{lapply} returns a list of the same length as \code{X}, each
  element of which is the result of applying \code{FUN} to the
  corresponding element of \code{X}.

  \code{sapply} is a user-friendly version and wrapper of \code{lapply}
  by default returning a vector, matrix or, if \code{simplify = "array"}, an
  array if appropriate, by applying \code{simplify2array()}.
  \code{sapply(x, f, simplify = FALSE, USE.NAMES = FALSE)} is the same as
  \code{lapply(x, f)}.

  \code{vapply} is similar to \code{sapply}, but has a pre-specified
  type of return value, so it can be safer (and sometimes faster) to
  use.

  \code{replicate} is a wrapper for the common use of \code{sapply} for
  repeated evaluation of an expression (which will usually involve
  random number generation).

  \code{simplify2array()} is the utility called from \code{sapply()}
  when \code{simplify} is not false and is similarly called from
  \code{\link{mapply}()}.
}
\details{
  \code{FUN} is found by a call to \code{\link{match.fun}} and typically
  is specified as a function or a symbol (e.g. a backquoted name) or a
  character string specifying a function to be searched for from the
  environment of the call to \code{lapply}.

  Function \code{FUN} must be able to accept as input any of the
  elements of \code{X}.  If the latter is an atomic vector, \code{FUN}
  will always be passed a length-one vector of the same type as \code{X}.

  Arguments in \code{\dots} cannot have the same name as any of the
  other arguments, and care may be needed to avoid partial matching to
  \code{FUN}.  In general-purpose code it is good practice to name the
  first two arguments \code{X} and \code{FUN} if \code{\dots} is passed
  through: this both avoids partial matching to \code{FUN} and ensures
  that a sensible error message is given if arguments named \code{X} or
  \code{FUN} are passed through \code{\dots}.

  Simplification in \code{sapply} is only attempted if \code{X} has
  length greater than zero and if the return values from all elements
  of \code{X} are all of the same (positive) length.  If the common
  length is one the result is a vector, and if greater than one is a
  matrix with a column corresponding to each element of \code{X}.

  Simplification is always done in \code{vapply}.  This function
  checks that all values of \code{FUN} are compatible with the
  \code{FUN.VALUE}, in that they must have the same length and type.
  (Types may be promoted to a higher type within the ordering logical
  < integer < double < complex, but not demoted.)

  Users of S4 classes should pass a list to \code{lapply} and
  \code{vapply}: the internal coercion is done by the \code{as.list} in
  the base namespace and not one defined by a user (e.g. by setting S4
  methods on the base function).
  %% rather should really use  as(x, "list")  iff  isS4(x)

  \code{lapply} and \code{vapply} are \link{primitive} functions.
}
\value{
  For \code{lapply}, \code{sapply(simplify = FALSE)} and
  \code{replicate(simplify = FALSE)}, a list.

  For \code{sapply(simplify = TRUE)} and \code{replicate(simplify =
  TRUE)}: if \code{X} has length zero or \code{n = 0}, an empty list.
  Otherwise an atomic vector or matrix or list of the same length as
  \code{X} (of length \code{n} for \code{replicate}).  If simplification
  occurs, the output type is determined from the highest type of the
  return values in the hierarchy NULL < raw < logical < integer < double <
  complex < character < list < expression, after coercion of pairlists
  to lists.

  \code{vapply} returns a vector or array of type matching the
  \code{FUN.VALUE}.  If \code{length(FUN.VALUE) == 1} a
  vector of the same length as \code{X} is returned, otherwise
  an array.  If \code{FUN.VALUE} is not an \code{\link{array}}, the
  result is a matrix   with \code{length(FUN.VALUE)} rows and
  \code{length(X)} columns, otherwise an array \code{a} with
  \code{\link{dim}(a) == c(dim(FUN.VALUE), length(X))}.

  The (Dim)names of the array value are taken from the \code{FUN.VALUE}
  if it is named, otherwise from the result of the first function call.
  Column names of the matrix or more generally the names of the last
  dimension of the array value or names of the vector value are set from
  \code{X} as in \code{sapply}.
}
\note{
  \code{sapply(*, simplify = FALSE, USE.NAMES = FALSE)} is
  equivalent to \code{lapply(*)}.

  For historical reasons, the calls created by \code{lapply} are
  unevaluated, and code has been written (e.g. \code{bquote}) that
  relies on this.  This means that the recorded call is always of the
  form \code{FUN(X[[0L]], ...)}, with \code{0L} replaced by the current
  integer index.  This is not normally a problem, but it can be if
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
lapply(x, mean)
# median and quartiles for each list element
lapply(x, quantile, probs = 1:3/4)
sapply(x, quantile)
i39 <- sapply(3:9, seq) # list of vectors
sapply(i39, fivenum)
vapply(i39, fivenum,
       c(Min. = 0, "1st Qu." = 0, Median = 0, "3rd Qu." = 0, Max. = 0))

## sapply(*, "array") -- artificial example
(v <- structure(10*(5:8), names = LETTERS[1:4]))
f2 <- function(x, y) outer(rep(x, length.out = 3), y)
(a2 <- sapply(v, f2, y = 2*(1:5), simplify = "array"))
a.2 <- vapply(v, f2, outer(1:3, 1:5), y = 2*(1:5))
stopifnot(dim(a2) == c(3,5,4), all.equal(a2, a.2),
          identical(dimnames(a2), list(NULL,NULL,LETTERS[1:4])))

hist(replicate(100, mean(rexp(10))))

## use of replicate() with parameters:
foo <- function(x = 1, y = 2) c(x, y)
# does not work: bar <- function(n, ...) replicate(n, foo(...))
bar <- function(n, x) replicate(n, foo(x = x))
bar(5, x = 3)
}
% taken from PR#8472
\keyword{iteration}
\keyword{list}
