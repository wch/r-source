% File src/library/base/man/utf8Conversion.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{utf8Conversion}
\alias{utf8ToInt}
\alias{intToUtf8}
\alias{Unicode}
\alias{code point}
\title{Convert to or from UTF-8-encoded Character Vectors}
\description{
  Conversion of UTF-8 encoded character vectors to and from integer vectors.
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
}
\value{
  Unicode defines a name and a number of all of the glyphs it
  encompasses: the numbers are called \emph{code points}: they run from
  \code{0} to \code{0x10FFFF}.
  
  \code{utf8ToInt} converts a length-one character string encoded in
  UTF-8 to an integer vector of Unicode code points.

  \code{intToUtf8} converts a numeric vector of Unicode code points
  either to a single character string or a character vector of single
  characters.  (For a single character string \code{0} is silently
  omitted: otherwise \code{0} is mapped to \code{""}.  Non-integral
  numeric values are truncated to integers.)  The \code{\link{Encoding}}
  is declared as \code{"UTF-8"}.

  \code{NA} inputs are mapped to \code{NA} output.
}
\examples{\donttest{
## will only display in some locales and fonts
intToUtf8(0x03B2L) # Greek beta
}}
\keyword{character}
\keyword{utilities}
