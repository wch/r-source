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
intToUtf8(x, multiple = FALSE)
}
\arguments{
  \item{x}{object to be converted.}
  \item{multiple}{logical: should the conversion be to a single
    character string or multiple individual characters?}
}
\details{
  These will work in any locale, including on platforms that do not
  otherwise support multi-byte character sets.

  Unicode defines a name and a number of all of the glyphs it
  encompasses: the numbers are called \emph{code points}: since RFC3629
  they run from \code{0} to \code{0x10FFFF} (with about 12\% being
  assigned by version 10.0 of the Unicode standard).
  
  \code{intToUtf8} does not handle surrogate pairs (which should not
  occur in UTF-32): inputs in the surrogate ranges are mapped to
  \code{NA}.
}
\value{
  \code{utf8ToInt} converts a length-one character string encoded in
  UTF-8 to an integer vector of Unicode code points.  It checks validity
  of the input.  (Currently it accepts UTF-8 encodings of code points
  greater than \code{0x10FFFF}: these are no longer regarded as valid by
  the UTF-8 RFC and will in future be mapped to \code{NA}.  Following
  \sQuote{Corrigendum 9} the UTF-8 encodings of the
  \sQuote{noncharacters} \code{0xFFFE} and \code{0xFFFF} are regarded as
  valid as from \R 3.4.3.)

  \code{intToUtf8} converts a numeric vector of Unicode code points
  either (default) to a single character string or a character vector of
  single characters.  Non-integral numeric values are truncated to
  integers: values above the maximum are mapped to \code{NA}.  For a
  single character string \code{0} is silently omitted: otherwise
  \code{0} is mapped to \code{""}.  The \code{\link{Encoding}} of a
  non-\code{NA} return value is declared as \code{"UTF-8"}.

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
}
\keyword{character}
\keyword{utilities}
