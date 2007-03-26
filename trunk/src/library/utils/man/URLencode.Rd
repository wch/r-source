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
  \item{reserved}{should reserved characters be encoded? See Details.}
}
\details{
  Characters in a URL other than the English alphanumeric characters and
  \code{$ - _ . + ! * ' ( ) ,} should be encoded as \code{\%}
  plus a two-digit hexadecimal representation, and any single-byte
  character can be so encoded. (Multi-byte characters are encoded as
  byte-by-byte.)

  In addition, \code{; / ? : @ = &} are reserved characters, and should
  be encoded unless used in their reserved sense, which is scheme
  specific.  The default in \code{URLencode} is to leave them alone, which
  is appropriate for \code{file://} URLs, but probably not for
  \code{http://} ones.
}
\value{
  A character string.
}
\references{
  RFC1738, \url{http://www.rfc-editor.org/rfc/rfc1738.txt}
}
\examples{
(y <- URLencode("a url with spaces and / and @"))
URLdecode(y)
(y <- URLencode("a url with spaces and / and @", reserved=TRUE))
URLdecode(y)
URLdecode("ab\%20cd")
}
\keyword{utilities}
