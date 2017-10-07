% File src/library/base/man/utf8Conversion.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{utf8Conversion}
\alias{utf8ToInt}
\alias{intToUtf8}
\alias{Unicode}
\alias{code point}
\title{Convert Integer Vectors to or from UTF-8-encoded Character Vectors}
\description{
  Conversion of UTF-8 encoded character vectors to and from integer
  vectors representing a UTF-32 encoding.
}
\usage{
utf8ToInt(x)
intToUtf8(x, multiple = FALSE, allow_surrogate_pairs = FALSE)
}
\arguments{
  \item{x}{object to be converted.}
  \item{multiple}{logical: should the conversion be to a single
    character string or multiple individual characters?}
  \item{allow_surrogate_pairs}{logical: should interpretation of
    surrogate pairs be attempted?  (See \sQuote{Details}.)
    Only supported for \code{multiple = FALSE} and in \R 3.5.0 and later.}
}
\details{
  These will work in any locale, including on platforms that do not
  otherwise support multi-byte character sets.

  Unicode defines a name and a number of all of the glyphs it
  encompasses: the numbers are called \emph{code points}: since RFC3629
  they run from \code{0} to \code{0x10FFFF} (with about 12\% being
  assigned by version 10.0 of the Unicode standard).
  
  \code{intToUtf8} does not by default handle surrogate pairs: inputs in
  the surrogate ranges are mapped to \code{NA}.  They might occur if a
  UTF-16 byte stream has been read as 2-byte integers (in the correct
  byte order), in which case \code{allow_surrogate_pairs = TRUE} will
  try to interpret them (with unmatched surrogate values still treated
  as \code{NA}).
}
\section{Validity}{
  Which code points are regarded as valid has changed over the lifetime
  of UTF-8.  Originally all 32-bit unsigned integers were potentially
  valid and could be converted to up to 6 bytes in UTF-8.  Since 2003 it
  has been stated that there will never be valid code points larger than
  \code{0x10FFFF}, and so valid UTF-8 encodings are never more than 4
  bytes.

  The code points in the surrogate-pair range \code{0xD000} to
  \code{0xDFFF} are prohibited in UTF-8 and so are regarded as invalid
  by \code{utf8ToInt} and by default by \code{intToUtf8}.

  The position of \sQuote{noncharacters} (notably \code{0xFFFE} and
  \code{0xFFFF}) was clarified by \sQuote{Corrigendum 9} in 2013.  These
  are valid but will never be given an official interpretation.  (In some
  earlier versions of \R \code{utf8ToInt} treated them as invalid.)
}
\value{
  \code{utf8ToInt} converts a length-one character string encoded in
  UTF-8 to an integer vector of Unicode code points.

  \code{intToUtf8} converts a numeric vector of Unicode code points
  either (default) to a single character string or a character vector of
  single characters. Non-integral numeric values are truncated to
  integers.  For output to a single character string \code{0} is
  silently omitted: otherwise \code{0} is mapped to \code{""}.  The
  \code{\link{Encoding}} of a non-\code{NA} return value is declared as
  \code{"UTF-8"}.

  Invalid and \code{NA} inputs are mapped to \code{NA} output.
}
\references{
  \url{https://tools.ietf.org/html/rfc3629}, the current standard for UTF-8.
  
  \url{http://www.unicode.org/versions/corrigendum9.html} for non-characters.
}
\examples{\donttest{
## will only display in some locales and fonts
intToUtf8(0x03B2L) # Greek beta
}
utf8ToInt("bi\u00dfchen")
utf8ToInt("\xfa\xb4\xbf\xbf\x9f")

## A valid UTF-16 surrogate pair (for U+10437)
x <- c(0xD801, 0xDC37)
intToUtf8(x)
intToUtf8(x, TRUE)
(xx <- intToUtf8(x, , TRUE)) # will only display in some locales and fonts
charToRaw(xx)

\dontrun{## An example of how surrogate pairs might occur
x <- "\U10437"
charToRaw(x)
foo <- tempfile()
writeLines(x, file(foo, encoding = "UTF-16LE"))
## next two are OS-specific, but are mandated by POSIX
system(paste("od -x", foo)) # 2-byte units, correct on little-endian platform
system(paste("od -t x1", foo)) # single bytes as hex
y <- readBin(foo, "integer", 2, 2, FALSE, endian = "little")
sprintf("\%X", y)
intToUtf8(y, , TRUE)
}
}
\keyword{character}
\keyword{utilities}
