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
\value{
These binary operators perform arithmetic on vector objects.
They return numeric vectors containing the result of the element
by element operations.  The elements of shorter vectors are recycled
as necessary.  The operators are \code{+} for addition, \code{-} for
subtraction \code{*} for multiplication, \code{/} for division and
\code{^} for exponentiation.
\code{\%\%} indicates \code{x mod y} and \code{\%/\%} indicates integer
division.

Objects such as arrays or time-series can be operated on this
way provided they are conformable.
}
\seealso{
\code{\link{Math}} for miscellaneous and \code{\link{Special}} for special
mathematical functions.
}
\examples{
x <- -1:12
x + 1
2 * x + 3
x \%\% 2
x \%/\% 5
}
\keyword{arith}
