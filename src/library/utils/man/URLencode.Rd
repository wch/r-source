% File src/library/utils/man/URLencode.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{URLencode}
\alias{URLencode}
\alias{URLdecode}
\title{Encode or Decode a (partial) URL}
\description{
  Functions to encode or decode characters in URLs.
}
\usage{
URLencode(URL, reserved = FALSE)
URLdecode(URL)
}
\arguments{
  \item{URL}{A character string.}
  \item{reserved}{should reserved characters be encoded?  See
    \sQuote{Details}.}
}
\details{
  Characters in a URL other than the English alphanumeric characters and
  \samp{- _ . ~} should be encoded as \code{\%}
  plus a two-digit hexadecimal representation, and any single-byte
  character can be so encoded. (Multi-byte characters are encoded
  byte-by-byte.)

  In addition, \samp{! $ & ' ( ) * + , ; = : / ? @ # [ ]} are reserved
  characters, and should be encoded unless used in their reserved sense,
  which is scheme specific.  The default in \code{URLencode} is to leave
  them alone, which is appropriate for \samp{file://} URLs, but probably
  not for \samp{http://} ones.

  \code{URLencode} will encode further the \code{\%} characters in an
  already-encoded URL. 
}
\value{
  A character string.
}
\references{
  RFC 3986 \url{http://tools.ietf.org/html/rfc3986}
}
\examples{
(y <- URLencode("a url with spaces and / and @"))
URLdecode(y)
(y <- URLencode("a url with spaces and / and @", reserved = TRUE))
URLdecode(y)
URLdecode("ab\%20cd")
}
\keyword{utilities}
