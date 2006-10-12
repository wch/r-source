\name{utf8Conversion}
\alias{utf8ToInt}
\alias{intToUtf8}
\title{Convert to or from UTF-8-enconded Character Vectors}
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
  These will work in any locale, including on machines that do not
  otherwise support multi-byte character sets.
}
\value{
  \code{utf8ToInt} converts a length-one character string encoded in
  UTF-8 to an integer vector of (numeric) UTF-8 code points.

  \code{intToUtf8} converts a vector of (numeric) UTF-8 code points
  either to a single character string or a character vector of single
  characters.
  (Note that a single character string could contain embedded nuls.)
}
\keyword{character}
\keyword{utilities}
