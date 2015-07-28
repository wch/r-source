% File src/library/base/man/rawConversion.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2004-2014 R Core Team
% Distributed under GPL 2 or later

\name{rawConversion}
\alias{charToRaw}
\alias{rawToChar}
\alias{rawShift}
\alias{rawToBits}
\alias{intToBits}
\alias{packBits}
\title{Convert to or from Raw Vectors}
\description{
  Conversion and manipulation of  objects of type \code{"raw"}.
}
\usage{
charToRaw(x)
rawToChar(x, multiple = FALSE)

rawShift(x, n)

rawToBits(x)
intToBits(x)
packBits(x, type = c("raw", "integer"))
}
\arguments{
  \item{x}{object to be converted or shifted.}
  \item{multiple}{logical: should the conversion be to a single
    character string or multiple individual characters?}
  \item{n}{the number of bits to shift.  Positive numbers shift right
    and negative numbers shift left: allowed values are \code{-8 ... 8}.}
  \item{type}{the result type.}
}
\value{
  \code{charToRaw} converts a length-one character string to raw bytes.
  It does so without taking into account any declared encoding (see
  \code{\link{Encoding}}).

  \code{rawToChar} converts raw bytes either to a single character
  string or a character vector of single bytes (with \code{""} for
  \code{0}).  (Note that a single character string could contain
  embedded nuls; only trailing nulls are allowed and will be removed.)
  In either case it is possible to create a result which is invalid in a
  multibyte locale, e.g.\sspace{}one using UTF-8. \link{Long vectors} are
  allowed if \code{multiple} is true.

  \code{rawShift(x, n)} shift the bits in \code{x} by \code{n} positions
  to the right, see the argument \code{n}, above.

  \code{rawToBits} returns a raw vector of 8 times the length of a raw
  vector with entries 0 or 1.  \code{intToBits} returns a raw vector
  of 32 times the length of an integer vector with entries 0 or 1.
  (Non-integral numeric values are truncated to integers.) In
  both cases the unpacking is least-significant bit first.

  \code{packBits} packs its input (using only the lowest bit for raw or
  integer vectors) least-significant bit first to a raw or integer vector.
}
\details{
  \code{packBits} accepts raw, integer or logical inputs, the last two
  without any NAs.
#ifdef unix

  Note that \sQuote{bytes} are not necessarily the same as characters,
  e.g.\sspace{}in UTF-8 locales.
#endif
}
\examples{
x <- "A test string"
(y <- charToRaw(x))
is.vector(y) # TRUE

rawToChar(y)
rawToChar(y, multiple = TRUE)
(xx <- c(y,  charToRaw("&"), charToRaw("more")))
rawToChar(xx)

rawShift(y, 1)
rawShift(y, -2)

rawToBits(y)

showBits <- function(r) stats::symnum(as.logical(rawToBits(r)))

z <- as.raw(5)
z ; showBits(z)
showBits(rawShift(z, 1)) # shift to right
showBits(rawShift(z, 2))
showBits(z)
showBits(rawShift(z, -1)) # shift to left
showBits(rawShift(z, -2)) # ..
showBits(rawShift(z, -3)) # shifted off entirely
}
\keyword{classes}
