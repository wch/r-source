% File src/library/base/man/rawConversion.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2004-2021 R Core Team
% Distributed under GPL 2 or later

\name{rawConversion}
\alias{charToRaw}
\alias{rawToChar}
\alias{rawShift}
\alias{rawToBits}
\alias{intToBits}
\alias{packBits}
\alias{numToInts}
\alias{numToBits}
\title{Convert to or from (Bit/Packed) Raw Vectors}
\description{
  Conversion to and from and manipulation of objects of type \code{"raw"},
  both used as bits or \dQuote{packed} 8 bits.
}
\usage{
charToRaw(x)
rawToChar(x, multiple = FALSE)

rawShift(x, n)

rawToBits(x)
intToBits(x)
packBits(x, type = c("raw", "integer", "double"))

numToInts(x)
numToBits(x)
}
\arguments{
  \item{x}{object to be converted or shifted.}
  \item{multiple}{logical: should the conversion be to a single
    character string or multiple individual characters?}
  \item{n}{the number of bits to shift.  Positive numbers shift right
    and negative numbers shift left: allowed values are \code{-8 ... 8}.}
  \item{type}{the result type, partially matched.}
}
\value{
  \code{charToRaw} converts a length-one character string to raw bytes.
  It does so without taking into account any declared encoding (see
  \code{\link{Encoding}}).

  \code{rawToChar} converts raw bytes either to a single character
  string or a character vector of single bytes (with \code{""} for
  \code{0}).  (Note that a single character string could contain
  embedded \abbr{NUL}s; only trailing nulls are allowed and will be removed.)
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
  integer vectors) least-significant bit first to a raw, integer or double
  (\dQuote{numeric}) vector.

  \code{numToInts()} and
  \code{numToBits()} split \code{\link{double}} precision numeric vectors
  either into to two \code{\link{integer}}s each or into 64 bits each,
  stored as \code{raw}.  In both cases the unpacking is least-significant
  element first.
}
\details{
  \code{packBits} accepts raw, integer or logical inputs, the last two
  without any NAs.

  \code{numToBits(.)} and \code{packBits(., type="double")} are
  \emph{inverse} functions of each other, see also the examples.

  Note that \sQuote{bytes} are not necessarily the same as characters,
  e.g.\sspace{}in UTF-8 locales.
}
\examples{
x <- "A test string"
(y <- charToRaw(x))
is.vector(y) # TRUE

rawToChar(y)
rawToChar(y, multiple = TRUE)
(xx <- c(y,  charToRaw("&"), charToRaw(" more")))
rawToChar(xx)

rawShift(y, 1)
rawShift(y,-2)

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

packBits(as.raw(0:31))
i <- -2:3
stopifnot(exprs = {
  identical(i, packBits(intToBits(i), "integer"))
  identical(packBits(       0:31) ,
            packBits(as.raw(0:31)))
})
str(pBi <- packBits(intToBits(i)))
data.frame(B = matrix(pBi, nrow=6, byrow=TRUE),
           hex = format(as.hexmode(i)), i)


## Look at internal bit representation of ...

## ... of integers :
bitI <- function(x) vapply(as.integer(x), function(x) {
            b <- substr(as.character(rev(intToBits(x))), 2L, 2L)
            paste0(c(b[1L], " ", b[2:32]), collapse = "")
          }, "")
print(bitI(-8:8), width = 35, quote = FALSE)

## ... of double precision numbers in format  'sign exp | mantissa'
## where  1 bit sign  1 <==> "-";
##       11 bit exp   is the base-2 exponent biased by 2^10 - 1 (1023)
##       52 bit mantissa is without the implicit leading '1'
#
## Bit representation  [ sign | exponent | mantissa ] of double prec numbers :

bitC <- function(x) noquote(vapply(as.double(x), function(x) { # split one double
    b <- substr(as.character(rev(numToBits(x))), 2L, 2L)
    paste0(c(b[1L], " ", b[2:12], " | ", b[13:64]), collapse = "")
  }, ""))
bitC(17)
bitC(c(-1,0,1))
bitC(2^(-2:5))
bitC(1+2^-(1:53))# from 0.5 converge to 1

###  numToBits(.)  <==>   intToBits(numToInts(.)) :
d2bI <- function(x) vapply(as.double(x), function(x) intToBits(numToInts(x)), raw(64L))
d2b  <- function(x) vapply(as.double(x), function(x)           numToBits(x) , raw(64L))
set.seed(1)
x <- c(sort(rt(2048, df=1.5)),  2^(-10:10), 1+2^-(1:53))
str(bx <- d2b(x)) # a  64 x 2122  raw matrix
stopifnot( identical(bx, d2bI(x)) )

## Show that  packBits(*, "double")  is the inverse of numToBits() :
packBits(numToBits(pi), type="double")
bitC(2050)
b <- numToBits(2050) 
identical(b, numToBits(packBits(b, type="double")))
pbx <- apply(bx, 2, packBits, type="double")
stopifnot( identical(pbx, x))
}
\keyword{classes}
