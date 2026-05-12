% File src/library/base/man/strtoi.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2009-2010 R Core Team
% Distributed under GPL 2 or later

\name{strtoi}
\alias{strtoi}
\title{Convert Strings to Integers}
\description{
  Convert strings to integers according to the given base using the C
  function \code{strtol}, or choose a suitable base following the C rules.
}
\usage{strtoi(x, base = 0L)}
\arguments{
  \item{x}{a character vector, or something coercible to this by
    \code{\link{as.character}}.}
  \item{base}{an integer which is between 2 and 36 inclusive, or zero
    (default).}
}
\details{
  Conversion is based on the C library function \code{strtol}.

  For the default \code{base = 0L}, the base chosen from the string
  representation of that element of \code{x}, so different elements can
  have different bases (see the first example).  The standard C rules
  for choosing the base are that octal constants (prefix \samp{0} not
  followed by \samp{x} or \samp{X}) and hexadecimal constants (prefix
  \samp{0x} or \samp{0X}) are interpreted as base \code{8} and
  \code{16}; all other strings are interpreted as base \code{10}.

  For a base greater than \code{10}, letters \samp{a} to \samp{z} (or
  \samp{A} to \samp{Z}) are used to represent \code{10} to \code{35}.
}
\value{
  An integer vector of the same length as \code{x}.  Values which cannot
  be interpreted as integers or would overflow are returned as
  \code{\link{NA_integer_}}.
}
\seealso{
  For decimal strings \code{\link{as.integer}} is equally useful.
}
\examples{
strtoi(c("0xff", "077", "123"))
strtoi(c("ffff", "FFFF"), 16L)
strtoi(c("177", "377"), 8L)
}
\keyword{classes}
\keyword{character}
\keyword{utilities}
