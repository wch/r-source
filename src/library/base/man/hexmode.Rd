% File src/library/base/man/hexmode.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2022 R Core Team
% Distributed under GPL 2 or later

\name{hexmode}
\title{Integer Numbers Displayed in Hexadecimal}
\alias{as.hexmode}
\alias{format.hexmode}
\alias{print.hexmode}
\alias{as.character.hexmode}
\alias{[.hexmode}
\alias{!.hexmode}
\alias{|.hexmode}
\alias{&.hexmode}
%% FIXME: xor() is not generic (yet?).
%% \alias{xor.hexmode}
\alias{hexmode}
\description{
  Integers which are displayed in hexadecimal (short \sQuote{hex}) format,
  with as many digits as are needed to display the largest, using leading
  zeroes as necessary.

  Arithmetic works as for integers, and non-integer valued mathematical
  functions typically work by truncating the result to integer.
}
\usage{
as.hexmode(x)

\method{as.character}{hexmode}(x, keepStr = FALSE, \dots)

\method{format}{hexmode}(x, width = NULL, upper.case = FALSE, \dots)

\method{print}{hexmode}(x, \dots)
}
\arguments{
  \item{x}{an object, for the methods inheriting from class \code{"hexmode"}.}
  \item{keepStr}{a \code{\link{logical}} indicating that names and
    dimensions should be kept; set \code{TRUE} for back compatibility, if needed.}
  \item{width}{\code{NULL} or a positive integer specifying the minimum
    field width to be used, with padding by leading zeroes.}
  \item{upper.case}{a logical indicating whether to use upper-case
    letters or lower-case letters (default).}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  Class \code{"hexmode"} consists of integer vectors with that class
  attribute, used primarily to ensure that they are printed in hex.
  Subsetting (\code{\link{[}}) works too, as do arithmetic or
  other mathematical operations, albeit truncated to integer.

  \code{as.character(x)} drops all \code{\link{attributes}} (unless when
  \code{keepStr=TRUE} where it keeps, \code{dim}, \code{dimnames} and
  \code{names} for back compatibility) and converts each entry individually, hence with no
  leading zeroes, whereas in \code{format()}, when \code{width = NULL} (the
  default), the output is padded with leading zeroes to the smallest width
  needed for all the non-missing elements.

  \code{as.hexmode} can convert integers (of \link{type} \code{"integer"} or
  \code{"double"}) and character vectors whose elements contain only
  \code{0-9}, \code{a-f}, \code{A-F} (or are \code{NA}) to class
  \code{"hexmode"}.

  There is a \code{\link{!}} method and methods for \code{\link{|}} and
  \code{\link{&}}:
  %% FIXME: xor() is not generic (yet?).
  %% and \code{\link{xor}}:
  these recycle their arguments to the length of the longer and then
  apply the operators bitwise to each element.
}
\seealso{
  \code{\link{octmode}}, \code{\link{sprintf}} for other options in
  converting integers to hex, \code{\link{strtoi}} to convert hex
  strings to integers.
}
\examples{
i <- as.hexmode("7fffffff")
i; class(i)
identical(as.integer(i), .Machine$integer.max)

hm <- as.hexmode(c(NA, 1)); hm
as.integer(hm)

Xm <- as.hexmode(1:16)
Xm # print()s via format()
stopifnot(nchar(format(Xm)) == 2)
Xm[-16] # *no* leading zeroes!
stopifnot(format(Xm[-16]) == c(1:9, letters[1:6]))

## Integer arithmetic (remaining "hexmode"):
16*Xm
Xm^2
-Xm
(fac <- factorial(Xm[1:12])) # !1, !2, !3, !4 .. in hexadecimals
as.integer(fac) # indeed the same as  factorial(1:12)
}
\keyword{utilities}
\keyword{print}
