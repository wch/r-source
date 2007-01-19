\name{NumericConstants}
\alias{NumericConstants}
\title{Numeric Constants}
\description{
  How \R parses numeric constants.
}
\details{
  \R parses numeric constants in its input in a very similar way to C
  floating-point constants.

  \code{\link{Inf}} and \code{\link{NaN}} are numeric constants (with
  \code{\link{typeof}(.) "double"}).  All other numeric constants start
  with a digit or period.

  Hexadecimal constants start with \code{0x} or \code{0X} followed by
  a non-empty sequence from \code{0-9 a-f A-F} which is interpreted as a
  hexadecimal number (\code{"double"}, not \code{"integer"}).

  Decimal constants consists of a nonempty sequence of digits possibly
  containing a period (the decimal point), optionally followed by a
  decimal exponent.  A decimal exponent consists of an \code{E} or
  \code{e} followed by an optional plus or minus sign followed by a
  non-empty sequence of digits, and indicates multiplication by a power
  of ten.

  A numeric constant immediately followed by \code{i} is regarded as an
  imaginary \link{complex} number.

  An numeric constant immediately followed by \code{L} is regarded as an
  \code{\link{integer}} number when possible (and with a warning if it
  contains a \code{"."}).

  Only the ASCII digits 0--9 are recognized as digits, even in languages
  which have other representations of digits.
  The \sQuote{decimal separator} is always a period and never a comma.

  Note that a leading plus or minus is not part of numeric constant but
  a unary operator applied to the constant.
}
\seealso{
  \code{\link{Syntax}}.

  \code{\link{Quotes}} for the parsing of character constants,
}
\examples{
2.1
typeof(2)
sqrt(1i) # remember elementary math?
str(0xA0)
identical(1L, as.integer(1))

## You can combine the "0x" prefix with the "L" suffix :
identical(0xFL, as.integer(15)) # (with a regard to Fritz :-)
}
\keyword{documentation}
