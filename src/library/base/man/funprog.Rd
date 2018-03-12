% File src/library/base/man/funprog.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later
\name{funprog}
\alias{Filter}
\alias{Find}
\alias{Map}
\alias{Negate}
\alias{Reduce}
\alias{Position}
\title{Common Higher-Order Functions in Functional Programming Languages}
\description{
  \code{Reduce} uses a binary function to successively combine the
  elements of a given vector and a possibly given initial value.
  \code{Filter} extracts the elements of a vector for which a predicate
  (logical) function gives true.  \code{Find} and \code{Position} give
  the first or last such element and its position in the vector,
  respectively.  \code{Map} applies a function to the corresponding
  elements of given vectors.  \code{Negate} creates the negation of a
  given function.
}
\usage{
Reduce(f, x, init, right = FALSE, accumulate = FALSE)
Filter(f, x)
Find(f, x, right = FALSE, nomatch = NULL)
Map(f, ...)
Negate(f)
Position(f, x, right = FALSE, nomatch = NA_integer_)
}
\arguments{
  \item{f}{a function of the appropriate arity (binary for
    \code{Reduce}, unary for \code{Filter}, \code{Find} and
    \code{Position}, \eqn{k}-ary for \code{Map} if this is called with
    \eqn{k} arguments).  An arbitrary predicate function for
    \code{Negate}.}
  \item{x}{a vector.}
  \item{init}{an \R object of the same kind as the elements of
    \code{x}.}
  \item{right}{a logical indicating whether to proceed from left to
    right (default) or from right to left.}
  \item{accumulate}{a logical indicating whether the successive reduce
    combinations should be accumulated.  By default, only the final
    combination is used.}
  \item{nomatch}{the value to be returned in the case when
    \dQuote{no match} (no element satisfying the predicate) is found.}
  \item{\dots}{vectors.}
}
\details{
  If \code{init} is given, \code{Reduce} logically adds it to the start
  (when proceeding left to right) or the end of \code{x}, respectively.
  If this possibly augmented vector \eqn{v} has \eqn{n > 1} elements,
  \code{Reduce} successively applies \eqn{f} to the elements of \eqn{v}
  from left to right or right to left, respectively.  I.e., a left
  reduce computes \eqn{l_1 = f(v_1, v_2)}, \eqn{l_2 = f(l_1, v_3)}, etc.,
  and returns \eqn{l_{n-1} = f(l_{n-2}, v_n)}, and a right reduce does
  \eqn{r_{n-1} = f(v_{n-1}, v_n)}, \eqn{r_{n-2} = f(v_{n-2}, r_{n-1})}
  and returns \eqn{r_1 = f(v_1, r_2)}.  (E.g., if \eqn{v} is the
  sequence (2, 3, 4) and \eqn{f} is division, left and right reduce give
  \eqn{(2 / 3) / 4 = 1/6} and \eqn{2 / (3 / 4) = 8/3}, respectively.)
  If \eqn{v} has only a single element, this is returned; if there are
  no elements, \code{NULL} is returned.  Thus, it is ensured that
  \code{f} is always called with 2 arguments.

  The current implementation is non-recursive to ensure stability and
  scalability.

  \code{Reduce} is patterned after Common Lisp's \code{reduce}.  A
  reduce is also known as a fold (e.g., in Haskell) or an accumulate
  (e.g., in the C++ Standard Template Library).  The accumulative
  version corresponds to Haskell's scan functions.

  \code{Filter} applies the unary predicate function \code{f} to each
  element of \code{x}, coercing to logical if necessary, and returns the
  subset of \code{x} for which this gives true.  Note that possible
  \code{NA} values are currently always taken as false; control over
  \code{NA} handling may be added in the future.  \code{Filter}
  corresponds to \code{filter} in Haskell or \code{remove-if-not} in
  Common Lisp.

  \code{Find} and \code{Position} are patterned after Common Lisp's
  \code{find-if} and \code{position-if}, respectively.  If there is an
  element for which the predicate function gives true, then the first or
  last such element or its position is returned depending on whether
  \code{right} is false (default) or true, respectively.  If there is no
  such element, the value specified by \code{nomatch} is returned.  The
  current implementation is not optimized for performance.

  \code{Map} is a simple wrapper to \code{\link{mapply}} which does not
  attempt to simplify the result, similar to Common Lisp's \code{mapcar}
  (with arguments being recycled, however).  Future versions may allow
  some control of the result type.

  \code{Negate} corresponds to Common Lisp's \code{complement}.  Given a
  (predicate) function \code{f}, it creates a function which returns the
  logical negation of what \code{f} returns.
}

\seealso{
  Function \code{\link{clusterMap}} and \code{\link{mcmapply}} (not
  Windows) in package \pkg{parallel} provide parallel versions of \code{Map}.
}

\examples{
## A general-purpose adder:
add <- function(x) Reduce("+", x)
add(list(1, 2, 3))
## Like sum(), but can also used for adding matrices etc., as it will
## use the appropriate '+' method in each reduction step.
## More generally, many generics meant to work on arbitrarily many
## arguments can be defined via reduction:
FOO <- function(...) Reduce(FOO2, list(...))
FOO2 <- function(x, y) UseMethod("FOO2")
## FOO() methods can then be provided via FOO2() methods.

## A general-purpose cumulative adder:
cadd <- function(x) Reduce("+", x, accumulate = TRUE)
cadd(seq_len(7))

## A simple function to compute continued fractions:
cfrac <- function(x) Reduce(function(u, v) u + 1 / v, x, right = TRUE)
## Continued fraction approximation for pi:
cfrac(c(3, 7, 15, 1, 292))
## Continued fraction approximation for Euler's number (e):
cfrac(c(2, 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8))

## Iterative function application:
Funcall <- function(f, ...) f(...)
## Compute log(exp(acos(cos(0))))
Reduce(Funcall, list(log, exp, acos, cos), 0, right = TRUE)
## n-fold iterate of a function, functional style:
Iterate <- function(f, n = 1)
    function(x) Reduce(Funcall, rep.int(list(f), n), x, right = TRUE)
## Continued fraction approximation to the golden ratio:
Iterate(function(x) 1 + 1 / x, 30)(1)
## which is the same as
cfrac(rep.int(1, 31))
## Computing square root approximations for x as fixed points of the
## function t |-> (t + x / t) / 2, as a function of the initial value:
asqrt <- function(x, n) Iterate(function(t) (t + x / t) / 2, n)
asqrt(2, 30)(10) # Starting from a positive value => +sqrt(2)
asqrt(2, 30)(-1) # Starting from a negative value => -sqrt(2)

## A list of all functions in the base environment:
funs <- Filter(is.function, sapply(ls(baseenv()), get, baseenv()))
## Functions in base with more than 10 arguments:
names(Filter(function(f) length(formals(f)) > 10, funs))
## Number of functions in base with a '...' argument:
length(Filter(function(f)
              any(names(formals(f)) \%in\% "..."),
              funs))
\donttest{
## Find all objects in the base environment which are *not* functions:
Filter(Negate(is.function),  sapply(ls(baseenv()), get, baseenv()))
}}
\keyword{programming}
