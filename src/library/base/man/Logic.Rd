\name{Logic}
\title{Logical Operators}
\usage{
! x
x & y
x && y
x | y
x || y
xor(x, y)
}
\alias{!}
\alias{&}
\alias{&&}
\alias{|}
\alias{||}
\alias{xor}
\alias{Logic}
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

  Numeric and complex vectors will be coerced to logical values, with
  zero being false and all non-zero values being true.  Raw vectors are
  handled without any coercion for \code{!}, \code{&} and \code{|}, with
  these operators being applied bitwise (so \code{!} is the 1-complement).
  
  The operators \code{!}, \code{&} and \code{|} are generic functions:
  methods can be written for them individually or via the
  \code{\link{Ops}}) group generic function.

  \code{\link{NA}} is a valid logical object.  Where a component of
  \code{x} or \code{y} is \code{NA}, the result will be \code{NA} if the
  outcome is ambiguous.  In other words \code{NA & TRUE} evaluates to
  \code{NA}, but \code{NA & FALSE} evaluates to \code{FALSE}.  See the
  examples below.
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
