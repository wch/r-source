\name{encodeString}
\alias{encodeString}
\title{
  Encode Character Vector as for Printing
}
\description{
  \code{encodeString} escapes the strings in a character vector in the
  same way \code{print.default} does, and optionally fits the encoded
  strings within a field width.
}
\usage{
encodeString(x, w = 0, quote = "", na = TRUE,
             justify = c("left", "right", "centre"))
}
\arguments{
  \item{x}{A character vector, or an object that can be coerced to one
    by \code{\link{as.character}}.}
  \item{w}{integer: the minimum field width.  If \code{NA}, this is
    taken to be the largest field width needed for any element of \code{x}.}
  \item{quote}{character: quoting character, if any.}
  \item{na}{logical: should \code{NA} strings be encoded?}
  \item{justify}{character: partial matches are allowed.  If padding to
    the minimum field width is needed, how should spaces be inserted?}
}
\details{
  This escapes backslash and the control characters \code{\a} (bell),
  \code{\b} (backspace), \code{\f} (formfeed), \code{\n} (line feed),
  \code{\r} (carriage return), \code{\t} (tab), \code{\v} (vertical tab)
  and \code{\0} (nul) as well as any non-printable characters, which are
  printed in octal notation (\code{\xyz} with leading zeroes).
#ifdef unix
  (Which characters are non-printable depends on the current locale.)
#endif
#ifdef windows
  (Windows' reporting of printable characters is unreliable, so all
  other control characters are regarded as non-printable, and all
  characters with codes 32--255 as printable.)
#endif

  If \code{quote} is a single or double quote any embedded quote of the
  same type is escaped.  Note that justification is of the quoted
  string, hence spaces are added outside the quotes.
}
\value{
  A character vector of the same length as \code{x}, with the same
  attributes (including names and dimensions).
}
\seealso{
  \code{\link{print.default}}
}
\examples{
x <- "ab\bc\ndef"
print(x)
cat(x) # interprets escapes
cat(encodeString(x), "\n", sep="") # similar to print()

factor(x) # makes use of this to print the levels

x <- c("a", "ab", "abcde")
encodeString(x, w = NA) # left justification
encodeString(x, w = NA, justify = "c")
encodeString(x, w = NA, justify = "r")
encodeString(x, w = NA, quote = "'", justify = "r")
}
\keyword{ utilities }

