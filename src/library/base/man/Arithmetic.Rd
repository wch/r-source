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
\description{
  These binary operators perform arithmetic on numeric or complex vectors
  (or objects which can be coerced to them).
}
\arguments{
  \item{x, y}{numeric or complex vectors or objects which can be
    coerced to such, or other objects for which methods have been written.}
}
\value{
  They return numeric vectors containing the result of the element
  by element operations.  The elements of shorter vectors are recycled
  as necessary (with a \code{\link{warning}} when they are recycled only
  \emph{fractionally}).  The operators are \code{+} for addition,
  \code{-} for subtraction, \code{*} for multiplication, \code{/} for
  division and \code{^} for exponentiation.

  \code{\%\%} indicates \code{x mod y} and \code{\%/\%} indicates
  integer division.  It is guaranteed that \code{x == (x \%\% y) + y * ( x
    \%/\% y )} unless \code{y == 0} where the result is \code{\link{NA}} or
  \code{\link{NaN}} (depending on the \code{\link{typeof}} of the
  arguments).
}
\details{
  The binary arithmetic operators are generic functions: methods can be
  written for them individually or via the \code{\link{Ops}}) group generic
  function.
  
  If applied to arrays the result will be an array if this is sensible
  (for example it will not if the recycling rule has been invoked).

  Logical vectors will be coerced to numeric vectors, \code{FALSE}
  having value 0 and \code{TRUE} having value one.
  
  \code{1 ^ y} and \code{y ^ 0} are \code{1}, \emph{always}.
  \code{x ^ y} should also give the proper \dQuote{limit} result when
  either argument is infinite (i.e., \code{+- \link{Inf}}).

  Objects such as arrays or time-series can be operated on this
  way provided they are conformable.
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
}
\examples{
x <- -1:12
x + 1
2 * x + 3
x \%\% 2 #-- is periodic
x \%/\% 5
}
\keyword{arith}
