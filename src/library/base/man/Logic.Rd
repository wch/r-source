% File src/library/base/man/Logic.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{Logic}
\title{Logical Operators}
\alias{!}
\alias{&}
\alias{&&}
\alias{|}
\alias{||}
\alias{xor}
\alias{Logic}
\alias{isTRUE}
\alias{isFALSE}
\usage{
! x
x & y
x && y
x | y
x || y
xor(x, y)

isTRUE (x)
isFALSE(x)
}
\description{
  These operators act on raw, logical and number-like vectors.
}
\arguments{
  \item{x, y}{\code{\link{raw}}, \code{\link{logical}} or \sQuote{number-like} vectors (i.e., of
    types \code{\link{double}} (class \code{\link{numeric}}),
    \code{\link{integer}} and \code{\link{complex}}), or objects for
    which methods have been written.}
}
\details{
  \code{!} indicates logical negation (NOT).

  \code{&} and \code{&&} indicate logical AND and \code{|} and \code{||}
  indicate logical OR.  The shorter form performs elementwise
  comparisons in much the same way as arithmetic operators.  The longer
  form evaluates left to right examining only the first element of each
  vector.  Evaluation proceeds only until the result is determined.  The
  longer form is appropriate for programming control-flow and typically
  preferred in \code{\link{if}} clauses.

  \code{xor} indicates elementwise exclusive OR.

  \code{isTRUE(x)} is the same as
  \code{{ is.logical(x) && length(x) == 1 && !is.na(x) && x }};
  \code{isFALSE()} is defined analogously.  Consequently,
  \code{if(isTRUE(cond))} may be preferable to \code{if(cond)} because
  of \code{\link{NA}}s.\cr
  In earlier \R versions, \code{isTRUE <- function(x) identical(x, TRUE)},
  had the drawback to be false e.g., for \code{x <- c(val = TRUE)}.

  Numeric and complex vectors will be coerced to logical values, with
  zero being false and all non-zero values being true.  Raw vectors are
  handled without any coercion for \code{!}, \code{&}, \code{|} and
  \code{xor}, with these operators being applied bitwise (so \code{!} is
  the 1s-complement).

  The operators \code{!}, \code{&} and \code{|} are generic functions:
  methods can be written for them individually or via the
  \code{\link[=S3groupGeneric]{Ops}} (or S4 \code{Logic}, see below)
  group generic function.  (See \code{\link[=S3groupGeneric]{Ops}} for
  how dispatch is computed.)

  \code{\link{NA}} is a valid logical object.  Where a component of
  \code{x} or \code{y} is \code{NA}, the result will be \code{NA} if the
  outcome is ambiguous.  In other words \code{NA & TRUE} evaluates to
  \code{NA}, but \code{NA & FALSE} evaluates to \code{FALSE}.  See the
  examples below.

  See \link{Syntax} for the precedence of these operators: unlike many
  other languages (including S) the AND and OR operators do not have the
  same precedence (the AND operators have higher precedence than the OR
  operators).
}
\value{
  For \code{!}, a logical or raw vector(for raw \code{x}) of the same
  length as \code{x}: names, dims and dimnames are copied from \code{x},
  and all other attributes (including class) if no coercion is done.

  For \code{|}, \code{&} and \code{xor} a logical or raw vector.  If
  involving a zero-length vector the result has length zero.  Otherwise,
  the elements of shorter vectors are recycled as necessary (with a
  \code{\link{warning}} when they are recycled only \emph{fractionally}).
  The rules for determining the attributes of the result are rather
  complicated.  Most attributes are taken from the longer argument, the
  first if they are of the same length.  Names will be copied from the
  first if it is the same length as the answer, otherwise from the
  second if that is.  For time series, these operations are allowed only
  if the series are compatible, when the class and \code{\link{tsp}}
  attribute of whichever is a time series (the same, if both are) are
  used.  For arrays (and an array result) the dimensions and dimnames
  are taken from first argument if it is an array, otherwise the second.

  For \code{||}, \code{&&} and \code{isTRUE}, a length-one logical vector.
}
\section{S4 methods}{
  \code{!}, \code{&} and \code{|} are S4 generics, the latter two part
  of the \code{\link[=S4groupGeneric]{Logic}} group generic (and
  hence methods need argument names \code{e1, e2}).
}
\note{
  The elementwise operators are sometimes called as functions as
  e.g.\sspace{}\code{`&`(x, y)}: see the description of how
  argument-matching is done in \code{\link[base]{Ops}}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{TRUE}} or \code{\link{logical}}.

  \code{\link{any}} and \code{\link{all}} for OR and AND on many scalar
  arguments.

  \code{\link{Syntax}} for operator precedence.

  \code{\link{bitwAnd}} for bitwise versions for integer vectors.
}
\examples{
y <- 1 + (x <- stats::rpois(50, lambda = 1.5) / 4 - 1)
x[(x > 0) & (x < 1)]    # all x values between 0 and 1
if (any(x == 0) || any(y == 0)) "zero encountered"

## construct truth tables :

x <- c(NA, FALSE, TRUE)
names(x) <- as.character(x)
outer(x, x, "&") ## AND table
outer(x, x, "|") ## OR  table
}
\keyword{logic}
