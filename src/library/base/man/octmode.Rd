% File src/library/base/man/octmode.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2022 R Core Team
% Distributed under GPL 2 or later

\name{octmode}
\title{Integer Numbers Displayed in Octal}
\alias{as.octmode}
\alias{format.octmode}
\alias{print.octmode}
\alias{as.character.octmode}
\alias{[.octmode}
\alias{!.octmode}
\alias{|.octmode}
\alias{&.octmode}
%% FIXME: xor() is not generic (yet?).
%% \alias{xor.octmode}
\alias{octmode}
\description{
  Integers which are displayed in octal (base-8 number system) format, with as
  many digits as are needed to display the largest, using leading zeroes as
  necessary.

  Arithmetic works as for integers, and non-integer valued mathematical
  functions typically work by truncating the result to integer.
}
\usage{
as.octmode(x)

\method{as.character}{octmode}(x, keepStr = FALSE, \dots)

\method{format}{octmode}(x, width = NULL, \dots)

\method{print}{octmode}(x, \dots)
}
\arguments{
  \item{x}{an object, for the methods inheriting from class \code{"octmode"}.}
  \item{keepStr}{a \code{\link{logical}} indicating that names and
    dimensions should be kept; set \code{TRUE} for back compatibility, if needed.}
  \item{width}{\code{NULL} or a positive integer specifying the minimum
    field width to be used, with padding by leading zeroes.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  \code{"octmode"} objects are integer vectors with that class
  attribute, used primarily to ensure that they are printed in octal
  notation, specifically for Unix-like file permissions such as
  \code{755}.  Subsetting (\code{\link{[}}) works too, as do arithmetic or
  other mathematical operations, albeit truncated to integer.

  \code{as.character(x)} drops all \code{\link{attributes}} (unless when
  \code{keepStr=TRUE} where it keeps, \code{dim}, \code{dimnames} and
  \code{names} for back compatibility) and converts each entry individually, hence with no
  leading zeroes, whereas in \code{format()}, when \code{width = NULL} (the
  default), the output is padded with leading zeroes to the smallest width
  needed for all the non-missing elements.

  \code{as.octmode} can convert integers (of \link{type} \code{"integer"} or
  \code{"double"}) and character vectors whose elements contain only
  digits \code{0-7} (or are \code{NA}) to class \code{"octmode"}.

  There is a \code{\link{!}} method and methods for \code{\link{|}} and
  \code{\link{&}}:
  %% FIXME: xor() is not generic (yet?).
  %% and \code{\link{xor}}:
  these recycle their arguments to the length of the longer and then
  apply the operators bitwise to each element.
}
\seealso{
  These are auxiliary functions for \code{\link{file.info}}.

  \code{\link{hexmode}}, \code{\link{sprintf}} for other options in
  converting integers to octal, \code{\link{strtoi}} to convert octal
  strings to integers.
}
\examples{
(on <- as.octmode(c(16, 32, 127:129))) # "020" "040" "177" "200" "201"
unclass(on[3:4]) # subsetting

## manipulate file modes
fmode <- as.octmode("170")
(fmode | "644") & "755"
\dontdiff{
(umask <- Sys.umask()) # depends on platform
c(fmode, "666", "755") & !umask
}

om <- as.octmode(1:12)
om # print()s via format()
stopifnot(nchar(format(om)) == 2)
om[1:7] # *no* leading zeroes!
stopifnot(format(om[1:7]) == as.character(1:7))
om2 <- as.octmode(c(1:10, 60:70))
om2 # prints via format() -> with 3 octals
stopifnot(nchar(format(om2)) == 3)
as.character(om2) # strings of length 1, 2, 3


## Integer arithmetic (remaining "octmode"):
om^2
om * 64
-om
(fac <- factorial(om)) # !1, !2, !3, !4 .. in hexadecimals
as.integer(fac) # indeed the same as  factorial(1:12)
}
\keyword{utilities}
\keyword{print}
