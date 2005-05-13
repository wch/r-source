\name{URLencode}
\alias{URLencode}
\alias{URLdecode}
\title{Encode or Decode a (partial) URL}
\description{
  Functions to encode or decode characters in URLs.
}
\usage{
URLencode(UR, reserved=TRUE)
URLdecode(URL)
}
\arguments{
  \item{URL}{A character string.}
  \item{reserved}{should reserved characters be encoded? See Details.}
}
\details{
  Characters in a URL other than the alphanumeric characters and
  \code{$ - _ . + ! * ' ( ) ,} should be encoded as \code{\%}
  plus a two-digit hexadecimal representation, and any character can be
  so encoded.
  
  In addition, \code{; / ? : @ = &} are reserved characters, and should
  be encoded unless used in their reserved sense, which is scheme
  specific.  Thie default in  \code{URLencode} is to encode them.
}
\value{
  A character string.
}
\references{
  RFC1738, \url{http://www.rfc-editor.org/rfc/rfc1738.txt}
}
\examples{
URLencode("a url with spaces and / and @")
URLdecode("ab\%20cd")
}
\keyword{utilities}
