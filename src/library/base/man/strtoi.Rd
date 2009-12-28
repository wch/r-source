% File src/library/base/man/strtoi.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2009 R Core Development Team
% Distributed under GPL 2 or later

\name{strtoi}
\alias{strtoi}
\title{Convert Strings to Integers}
\description{
  Convert strings to integers accordings to the given base using the C
  function \code{strtol}.
}
\usage{strtoi(x, base = 0L)}
\arguments{
  \item{x}{a character vector, or something coercible to this.}
  \item{base}{an integer which is between 2 and 36 inclusive, or zero
    (default).}
}
\details{
  Conversion is based on the C library function \code{strtol}: on
  failure or integer overflow, \code{\link{NA_integer_}} is returned.
}
\examples{
strtoi(c("0xff", "077", "123"))
strtoi(c("ffff", "FFFF"), 16L)
strtoi(c("177", "377"), 8L)
}
\keyword{classes}
\keyword{character}
\keyword{utilities}
