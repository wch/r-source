\name{raw}
\alias{raw}
\alias{as.raw}
\alias{charToRaw}
\alias{rawToChar}
\alias{rawShift}
\alias{rawToBits}
\alias{intToBits}
\alias{packBits}
\title{Raw Vectors}
\description{
  Creates or tests for objects of type \code{"raw"}.
}
\usage{
raw(length = 0)
as.raw(x)

charToRaw(x)
rawToChar(x, multiple=FALSE)

rawShift(x, n)

rawToBits(x)
intToBits(x)
packBits(x, type=c("raw", "integer"))
}
\arguments{
  \item{length}{desired length.}
  \item{x}{object to be coerced or tested.}
  \item{multiple}{logical: should the conversion be to a single
    character string or multiple individual characters?}
  \item{n}{the number of bits to shift.  Positive numbers shift right
    and negative numbers shift left: allowed values are \code{-8 ... 8}.}
  \item{type}{the result type.}
}
\value{
  \code{raw} creates a raw vector of the specified length.
  Each element of the vector is equal to \code{0}.
  Raw vectors are used to store fixed-length sequences of bytes.

  \code{as.raw} attempts to coerce its argument to be of raw
  type.  The (elementwise) answer will be \code{0} unless the
  coercion succeeds.

  \code{charToRaw} converts a length-one character string to raw bytes.
  \code{rawToChar} converts raw bytes either to a single character
  string or a character vector of single bytes.  (Note that a single
  character string could contain embedded nuls.)

  \code{rawToBits} returns a raw vector of 8 times the length of a raw
  vector with entries 0 or 1.  \code{intToBits} returns a raw vector
  of 32 times the length of an integer vector with entries 0 or 1. In
  both cases the packing is least-significant bit first.

  \code{packbits} packs its input (using only the lowest bit for raw or
  integer vectors) to a raw or integer vector.
}
\details{
  The raw type is intended to hold raw bytes.  It is possible to extract
  subsequences of bytes, and to replace elements (but only by elements
  of a raw vector).  The relational operators (see \link{Comparison})
  work, as do the logical operators (see \link{Logic}) with a bitwise
  interpretation.

  A raw vector is printed with each byte separately represented as a
  pair of hex digits.  If you want to see a character representation
  (with escape sequences for non-printing characters) use
  \code{rawToChar}.

  \code{packBits} accepts raw, integer or logical inputs, the last two
  without an NAs.
}
\examples{
x <- "A test string"
(y <- charToRaw(x))
is.vector(y) # TRUE

rawToChar(y)
rawToChar(y, multiple = TRUE)
(xx <- c(y, as.raw(0), charToRaw("more")))
rawToChar(xx)
xxx <- xx
xxx[length(y)+1] <- charToRaw("&")
xxx
rawToChar(xxx)

rawShift(y, 1)
rawShift(y, -2)

rawToBits(y)

## tests of binary read/write
zz <- file("testbin", "wb")
writeBin(xx, zz)
close(zz)
zz <- file("testbin", "rb")
(yy <- readBin(zz, "raw", 100))
seek(zz, 0, "start")
readBin(zz, "integer", n=100, size = 1) # read as small integers
seek(zz, 0, "start")
readBin(zz, "character", 100)  # is confused by embedded nul.
seek(zz, 0, "start")
readChar(zz, length(xx)) # correct
close(zz)
unlink("testbin")

## tests of ASCII read/write.
cat(xx, file="testascii")
scan("testascii", what=raw(0))

## tests of logic operators
!y
y & as.raw("0x0F")
y | as.raw("0x80")
}
\keyword{classes}
