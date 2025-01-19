% File src/library/base/man/sets.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2024 R Core Team
% Distributed under GPL 2 or later

\name{sets}
\alias{union}
\alias{intersect}
\alias{setdiff}
\alias{is.element}
\alias{setequal}
\concept{set operations}
\concept{sets} % not that useful for searches, but requested in a bug report
\alias{intersection}
\concept{difference}

\title{Set Operations}
\usage{
union(x, y)
intersect(x, y)
setdiff(x, y)
setequal(x, y)

is.element(el, set)
}
\arguments{
  \item{x, y, el, set}{vectors (of the same mode) containing a sequence
    of items (conceptually) with no duplicated values.}
}
\description{
  Performs \bold{set} union, intersection, (asymmetric!) difference,
  equality and membership on two vectors.
}
\details{
  The set operations are intended for \dQuote{same-kind}
  \dQuote{vector-like} objects containing sequences of items.  However,
  being \dQuote{vector-like} cannot easily be ascertained (in particular
  as \code{\link{is.vector}()} enforces a very narrow concept of
  \dQuote{vector}).

  Thus, for \R < 4.5.0, the set operands were always transformed via
  \code{\link{as.vector}()} (so that in particular, factors were coerced
  to character vectors).  Starting with \R 4.5.0, operands of the
  \dQuote{same kind} (in the sense that \code{\link{isa}(x, class(y))}
  or \code{\link{isa}(y, class(x))} which appear to be
  \dQuote{vector-like} (in the sense that \code{dim(x)} has length at
  most one, and subscripting \code{x} and/or
  \code{y} by \code{0L} leaves the class unchanged) are no longer
  transformed.   In particular, union, intersection and set difference
  of two factors now give factors (see the examples). 
  
  \code{is.element(x, y)} is identical to \code{x \%in\% y} (after
  possibly transforming via \code{\link{as.vector}()}).
}
\value{
  For \code{union}, a vector of a common mode or class.

  For \code{intersect}, a vector of a common mode or class, or
  \code{NULL} if \code{x} or \code{y} is \code{NULL}.

  For \code{setdiff}, a vector of the same \code{\link{mode}} or class
  as \code{x}.

  A logical scalar for \code{setequal} and a logical of the same
  length as \code{x} for \code{is.element}.
}
\seealso{
  \code{\link{\%in\%}}

  \sQuote{\link{plotmath}} for the use of \code{union} and
  \code{intersect} in plot annotation.
}

\examples{
(x <- c(sort(sample(1:20, 9)), NA))
(y <- c(sort(sample(3:23, 7)), NA))
union(x, y)
intersect(x, y)
setdiff(x, y)
setdiff(y, x)
setequal(x, y)

## True for all possible x & y :
setequal( union(x, y),
          c(setdiff(x, y), intersect(x, y), setdiff(y, x)))

is.element(x, y) # length 10
is.element(y, x) # length  8

## Factors:
x <- as.factor(c("A", "B", "A"))
y <- as.factor(c("B", "b"))
union(x, y)
intersect(x, y)
setdiff(x, y)
setdiff(y, x)
## (Note that union() and intersect() merge the levels.)
}
\keyword{misc}
