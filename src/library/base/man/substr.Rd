\name{substr}
\title{Extract Substrings from a Character Vector}
\usage{
substr(x, start, stop)
substring(text, first, last = 1000000)
}
\alias{substr}
\alias{substring}
\description{
  \code{substr} takes a character vector as an argument and returns a
  vector whose elements contain the substring starting with the
  character at position \code{start} up to the character at position
  \code{stop}. If \code{start} is larger than the string length then
  \code{NA} is returned. If \code{stop} is longer than \code{start} 
  and error is signalled.

  \code{substring} is compatible with S.  For vector arguments, it
  expands the arguments cyclically.
}
\seealso{
  \code{\link{paste}}, \code{\link{nchar}}, \code{\link{strsplit}}
}
\examples{
x <- c("asfef", "qwerty", "yuiop[", "b", "stuff.blah.yech")
substr(x, 3, 5)
substring(x, 3, 5)
substring(x, 3, 3:5)
}
