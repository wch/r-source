\name{encodeString}
\alias{encodeString}
\title{ Encode Character Vector as for Printing }
\description{
  \code{encodeString} escapes the strings in a character vector in the
  same way \code{print.default} does, and optionally fits the encoded
  string within a field width. 
}
\usage{
encodeString(x, w = 0, quote = "", right = FALSE, na = TRUE)
}
\arguments{
  \item{x}{A character vector, or an object that can be coerced to one
    by \code{\link{as.character}}.}
  \item{w}{integer: the minimum field width.}
  \item{quote}{character: quoting character, if any.}
  \item{right}{logical: if the escaped string is shorter than \code{w}
    characters, should it be left or right justified (by padding with
    spaces)?}
  \item{na}{logical: should \code{NA} strings be encoded?}
}
\details{
  This escapes the control characters \code{\a} (bell), \code{\b}
  (backspace), \code{\f} (formfeed), \code{\n} (line feed), \code{\r}
  (carriage return), \code{\t} (tab), \code{\v} (vertical tab) and
  \code{\0} (nul) as well as backslash and non-printable characters,
  which are printed in octal notation (\code{xyz} with leading zeroes).
#ifdef unix
  (Which characters are non-printable depends on the current locale.)
#endif
#ifdef windows
  (Windows is unreliable, so all other control characters are regarded
  as non-printable, and all characters with codes 32--255 as printable.)
#endif

  If \code{quote} is a single or double quote any embedded quote of the
  same type is escaped.
}
\value{
  A character vector of the same length as \code{x}, with the same attributes.
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
}
\keyword{ utilities }

