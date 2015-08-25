% File src/library/base/man/NumericConstants.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{NumericConstants}
\alias{NumericConstants}
\alias{1L}
\alias{0x1}
\alias{1i}
\title{Numeric Constants}
\description{
  How \R parses numeric constants.
}
\details{
  \R parses numeric constants in its input in a very similar way to C99
  floating-point constants.

  \code{\link{Inf}} and \code{\link{NaN}} are numeric constants (with
  \code{\link{typeof}(.) "double"}).  In text input (e.g., in
  \code{\link{scan}} and \code{\link{as.double}}), these are recognized
  ignoring case as is \code{infinity} as an alternative to \code{Inf}.
  \code{\link{NA_real_}} and \code{\link{NA_integer_}} are constants of
  types \code{"double"} and \code{"integer"} representing missing
  values.  All other numeric constants start with a digit or period and
  are either a decimal or hexadecimal constant optionally followed by
  \code{L}.

  Hexadecimal constants start with \code{0x} or \code{0X} followed by
  a nonempty sequence from \code{0-9 a-f A-F .} which is interpreted as a
  hexadecimal number, optionally followed by a binary exponent.  A binary
  exponent consists of a \code{P} or \code{p} followed by an optional
  plus or minus sign followed by a non-empty sequence of (decimal)
  digits, and indicates multiplication by a power of two.  Thus
  \code{0x123p456} is \eqn{291 \times 2^{456}}{291 * 2^456}.

  Decimal constants consist of a nonempty sequence of digits possibly
  containing a period (the decimal point), optionally followed by a
  decimal exponent.  A decimal exponent consists of an \code{E} or
  \code{e} followed by an optional plus or minus sign followed by a
  non-empty sequence of digits, and indicates multiplication by a power
  of ten.

  Values which are too large or too small to be representable will
  overflow to \code{Inf} or underflow to \code{0.0}.

  A numeric constant immediately followed by \code{i} is regarded as an
  imaginary \link{complex} number.

  An numeric constant immediately followed by \code{L} is regarded as an
  \code{\link{integer}} number when possible (and with a warning if it
  contains a \code{"."}).

  Only the ASCII digits 0--9 are recognized as digits, even in languages
  which have other representations of digits.  The \sQuote{decimal
    separator} is always a period and never a comma.

  Note that a leading plus or minus is not regarded by the parser as
  part of a numeric constant but as a unary operator applied to the constant.
}
\note{
  When a string is parsed to input a numeric constant, the number may or
  may not be representable exactly in the C double type used.  If not
  one of the nearest representable numbers will be returned.

  \R's own C code is used to convert constants to binary numbers, so the
  effect can be expected to be the same on all platforms implementing
  full IEC 600559 arithmetic (the most likely area of difference being
  the handling of numbers less than \code{\link{.Machine}$double.xmin}).
  The same code is used by \code{\link{scan}}.
}
\seealso{
  \code{\link{Syntax}}.
  For complex numbers, see \code{\link{complex}}.
  \code{\link{Quotes}} for the parsing of character constants,
  \code{\link{Reserved}} for the \dQuote{reserved words} in \R{}.
}
\examples{
## You can create numbers using fixed or scientific formatting.
2.1
2.1e10
-2.1E-10

## The resulting objects have class numeric and type double.
class(2.1)
typeof(2.1)

## This holds even if what you typed looked like an integer.
class(2)
typeof(2)

## If you actually wanted integers, use an "L" suffix.
class(2L)
typeof(2L)

## These are equal but not identical
2 == 2L
identical(2, 2L)

## You can write numbers between 0 and 1 without a leading "0"
## (but typically this makes code harder to read)
.1234

sqrt(1i) # remember elementary math?
utils::str(0xA0)
identical(1L, as.integer(1))

## You can combine the "0x" prefix with the "L" suffix :
identical(0xFL, as.integer(15))
}
\keyword{documentation}
