% File src/library/base/man/strtrim.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{strtrim}
\alias{strtrim}
\title{Trim Character Strings to Specified Display Widths}
\description{
  Trim character strings to specified display widths.
}
\usage{
strtrim(x, width)
}
\arguments{
  \item{x}{a character vector, or an object which can be coerced to a
    character vector by \code{\link{as.character}}.}
  \item{width}{Positive integer values: recycled to the length of \code{x}.}
}
\details{
  \sQuote{Width} is interpreted as the display width in a monospaced
  font.  What happens with non-printable characters (such as backspace, tab)
  is implementation-dependent and may depend on the locale (e.g., they
  may be included in the count or they may be omitted).

  Using this function rather than \code{\link{substr}} is important when
  there might be double-width (e.g., Chinese/Japanese/Korean) characters
  in the character vector.
}
\value{
  A character vector of the same length and with the same attributes
  as \code{x} (after possible coercion).

  Elements of the result will have the encoding declared as that of
  the current locale (see \code{\link{Encoding}}) if the corresponding
  input had a declared encoding and the current locale is either Latin-1
  or UTF-8.
}

\examples{
strtrim(c("abcdef", "abcdef", "abcdef"), c(1,5,10))
}
\keyword{ character }
\keyword{ utilities }
