\name{strtrim}
\alias{strtrim}
\title{Trim Character Strings to Specified Widths}
\description{
  Trim character strings to specified display widths.
}
\usage{
strtrim(x, width)
}
\arguments{
  \item{x}{a character vector, or an object which can be coerced to a
    character vector.}
  \item{width}{Positive integer values: recycled to the length of \code{x}.}
}
\details{
  \sQuote{Width} is interpreted as the display width in a monospaced
  font.  What happens with non-printable characters (such as backspace, tab)
  is implementation-dependent and may depend on the locale (e.g. they
  may be included in the count or they may be omitted).

  Using this function rather than \code{\link{substr}} is important when
  there might be double-width characters in character vectors
}
\value{
  A character vector of the same length as \code{x}.
}

\examples{
strtrim(c("abcdef", "abcdef", "abcdef"), c(1,5,10))
}
\keyword{ character }
\keyword{ utilities }
