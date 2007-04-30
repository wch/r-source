\name{Logic}
\title{Logical Operators}
\usage{
! x
x & y
x && y
x | y
x || y
xor(x, y)

isTRUE(x)
}
\alias{!}
\alias{&}
\alias{&&}
\alias{|}
\alias{||}
\alias{xor}
\alias{Logic}
\alias{isTRUE}
\description{
  These operators act on logical vectors.
}
\arguments{
  \item{x, y}{logical vectors, or objects which can be coerced to such
    or for which methods have been written.}
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

  \code{isTRUE(x)} is an abbreviation of \code{identical(TRUE, x)}, and
  so is true if and only if \code{x} is a length-one logical vector with
  no attributes (not even names).

  Numeric and complex vectors will be coerced to logical values, with
  zero being false and all non-zero values being true.  Raw vectors are
  handled without any coercion for \code{!}, \code{&} and \code{|}, with
  these operators being applied bitwise (so \code{!} is the 1-complement).

  The operators \code{!}, \code{&} and \code{|} are generic functions:
  methods can be written for them individually or via the
  \code{\link[base:groupGeneric]{Ops}}) group generic function.  (See
  \code{\link[base:groupGeneric]{Ops}} for how dispatch is computed.)

  \code{\link{NA}} is a valid logical object.  Where a component of
  \code{x} or \code{y} is \code{NA}, the result will be \code{NA} if the
  outcome is ambiguous.  In other words \code{NA & TRUE} evaluates to
  \code{NA}, but \code{NA & FALSE} evaluates to \code{FALSE}.  See the
  examples below.

  See \link{Syntax} for the precedence of these operators: unlike many
  other languages (including S) the AND and OR operators do not have the
  same precedence (the AND operators are higher than the OR operators).
}
\value{
  For \code{!}, a logical or raw vector of the same length as \code{x}.
  
  For \code{|}, \code{&} and \code{xor} a logical or raw vector. The
  elements of shorter vectors are recycled as necessary (with a
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
\section{S4 Methods}{
  \code{!}, \code{&} and \code{|} are S4 generics, the latter two part
  of the \code{Logic} group generic.  For historical reasons, the
  methods need argument names \code{e1} for \code{!} and \code{e1, e2}
  for the other two.

  \strong{NB} the signature for \code{!} methods will change to \code{x}
  in \R 2.6.0.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{TRUE}} or \code{\link{logical}}.

  \code{\link{any}} and \code{\link{all}} for OR and AND on many scalar
  arguments.

  \code{\link{Syntax}} for operator precedence.
}
\examples{
y <- 1 + (x <- rpois(50, lambda=1.5) / 4 - 1)
x[(x > 0) & (x < 1)]    # all x values between 0 and 1
if (any(x == 0) || any(y == 0)) "zero encountered"

## construct truth tables :

x <- c(NA, FALSE, TRUE)
names(x) <- as.character(x)
outer(x, x, "&")## AND table
outer(x, x, "|")## OR  table
}
\keyword{logic}
