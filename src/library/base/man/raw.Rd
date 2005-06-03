\name{raw}
\alias{raw}
\alias{as.raw}
\title{Raw Vectors}
\description{
  Creates or tests for objects of type \code{"raw"}.
}
\usage{
raw(length = 0)
as.raw(x)
}
\arguments{
  \item{length}{desired length.}
  \item{x}{object to be coerced.}
}
\value{
  \code{raw} creates a raw vector of the specified length.
  Each element of the vector is equal to \code{0}.
  Raw vectors are used to store fixed-length sequences of bytes.

  \code{as.raw} attempts to coerce its argument to be of raw
  type.  The (elementwise) answer will be \code{0} unless the
  coercion succeeds.
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
  \code{\link{rawToChar}}.
}
\seealso{
  \code{\link{rawToBits}}, \code\link{packShift}}, etc.
}
\examples{
xx <- raw(2)
xx[1] <- as.raw(40)     # NB, not just 40.
xx[2] <- charToRaw("A")
xx

x <- "A test string"
(y <- charToRaw(x))
is.vector(y) # TRUE
rawToChar(y)

isASCII <-  function(txt) all(charToRaw(txt) <= as.raw(127))
isASCII(x)  # true
isASCII("\x9c25.63") # false  (in Latin-1, this is an amount in UK pounds)
}
\keyword{classes}
