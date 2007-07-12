\name{Arithmetic}
\title{Arithmetic Operators}
\usage{
x + y
x - y
x * y
x / y
x ^ y
x \%\% y
x \%/\% y
}
\alias{+}
\alias{-}
\alias{*}
\alias{/}
\alias{^}
\alias{\%\%}
\alias{\%/\%}
\alias{Arithmetic}
\concept{remainder}
\concept{modulo}
\concept{modulus}
\concept{quotient}
\concept{division}
\description{
  These binary operators perform arithmetic on numeric or complex vectors
  (or objects which can be coerced to them).
}
\arguments{
  \item{x, y}{numeric or complex vectors or objects which can be
    coerced to such, or other objects for which methods have been written.}
}
\value{
  These operators return vectors containing the result of the element
  by element operations.  The elements of shorter vectors are recycled
  as necessary (with a \code{\link{warning}} when they are recycled only
  \emph{fractionally}).  The operators are \code{+} for addition,
  \code{-} for subtraction, \code{*} for multiplication, \code{/} for
  division and \code{^} for exponentiation.

  \code{\%\%} indicates \code{x mod y} and \code{\%/\%} indicates
  integer division.  It is guaranteed that \code{x == (x \%\% y) + y * ( x
    \%/\% y )} (up to rounding error) unless \code{y == 0}
  where the result is \code{\link{NA}} or \code{\link{NaN}} (depending
  on the \code{\link{typeof}} of the arguments).

  If either argument is complex the result will be complex, and if one
  or both arguments are numeric, the result will be numeric.  If both
  arguments are integer, the result of \code{/} and \code{^} is numeric
  and of the other operators integer (with overflow returned as
  \code{NA} with a warning).

  The rules for determining the attributes of the result are rather
  complicated.  Most attributes are taken from the longer argument, the
  first if they are of the same length.  Names will be copied from the
  first if it is the same length as the answer, otherwise from the
  second if that is.  For time series, these operations are allowed only
  if the series are compatible, when the class and \code{\link{tsp}}
  attribute of whichever is a time series (the same, if both are) are
  used.  For arrays (and an array result) the dimensions and dimnames
  are taken from first argument if it is an array, otherwise the second.
}
\details{
  The binary arithmetic operators are generic functions: methods can be
  written for them individually or via the
  \code{\link[base:groupGeneric]{Ops}} group generic function.  (See
  \code{\link[base:groupGeneric]{Ops}} for how dispatch is computed.)

  If applied to arrays the result will be an array if this is sensible
  (for example it will not if the recycling rule has been invoked).

  Logical vectors will be coerced to numeric vectors, \code{FALSE}
  having value 0 and \code{TRUE} having value one.

  \code{1 ^ y} and \code{y ^ 0} are \code{1}, \emph{always}.
  \code{x ^ y} should also give the proper \dQuote{limit} result when
  either argument is infinite (i.e., \code{+- \link{Inf}}).

  Objects such as arrays or time-series can be operated on this
  way provided they are conformable.

  For real arguments, \code{\%\%} can be subject to catastrophic loss of
  accuracy if \code{x} is much larger than \code{y}, and a warning is
  given if this is detected.
}
\section{S4 methods}{
  These operators are members of the S4 \code{\link{Arith}} group generic,
  and so methods can be written for them individually as well as for the
  group generic (or the \code{Ops} group generic), with arguments
  \code{c(e1, e2)}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{sqrt}} for miscellaneous and \code{\link{Special}} for special
  mathematical functions.

  \code{\link{Syntax}} for operator precedence.

  \code{\link{\%*\%}} for matrix multiplication.
}
\examples{
x <- -1:12
x + 1
2 * x + 3
x \%\% 2 #-- is periodic
x \%/\% 5
}
\keyword{arith}
