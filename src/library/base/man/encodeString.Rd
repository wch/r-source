% File src/library/base/man/encodeString.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2008, 2015 R Core Team
% Distributed under GPL 2 or later

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
encodeString(x, width = 0, quote = "", na.encode = TRUE,
             justify = c("left", "right", "centre", "none"))
}
\arguments{
  \item{x}{A character vector, or an object that can be coerced to one
    by \code{\link{as.character}}.}
  \item{width}{integer: the minimum field width.  If \code{NULL} or
    \code{NA}, this is taken to be the largest field width needed for
    any element of \code{x}.}
  \item{quote}{character: quoting character, if any.}
  \item{na.encode}{logical: should \code{NA} strings be encoded?}
  \item{justify}{character: partial matches are allowed.  If padding to
    the minimum field width is needed, how should spaces be inserted?
    \code{justify == "none"} is equivalent to \code{width = 0}, for
    consistency with \code{format.default}.}
}
\details{
  This escapes backslash and the control characters \samp{\\a} (bell),
  \samp{\\b} (backspace), \samp{\\f} (formfeed), \samp{\\n} (line feed),
  \samp{\\r} (carriage return), \samp{\\t} (tab) and \samp{\\v}
  (vertical tab) as well as any non-printable characters in a
  single-byte locale, which are printed in octal notation (\samp{\\xyz}
  with leading zeroes).

  Which characters are non-printable depends on the current locale.
  Windows' reporting of printable characters is unreliable, so there all
  other control characters are regarded as non-printable, and all
  characters with codes 32--255 as printable in a single-byte locale.
  See \code{\link{print.default}} for how non-printable characters are
  handled in multi-byte locales.

  If \code{quote} is a single or double quote any embedded quote of the
  same type is escaped.  Note that justification is of the quoted
  string, hence spaces are added outside the quotes.
}
\note{
  The default for \code{width} is different from \code{format.default},
  which does similar things for character vectors but without encoding
  using escapes.
}
\value{
  A character vector of the same length as \code{x}, with the same
  attributes (including names and dimensions) but with no class set.

  As from \R 3.0.0, marked UTF-8 encodings are preserved.
}
\seealso{
  \code{\link{print.default}}
}
\examples{
x <- "ab\bc\ndef"
print(x)
cat(x) # interprets escapes
cat(encodeString(x), "\n", sep = "") # similar to print()

factor(x) # makes use of this to print the levels

x <- c("a", "ab", "abcde")
encodeString(x) # width = 0: use as little as possible
encodeString(x, 2) # use two or more (left justified)
encodeString(x, width = NA) # left justification
encodeString(x, width = NA, justify = "c")
encodeString(x, width = NA, justify = "r")
encodeString(x, width = NA, quote = "'", justify = "r")
}
\keyword{ utilities }
