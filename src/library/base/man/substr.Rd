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
 \code{\link{strsplit}}, \code{\link{paste}}, \code{\link{nchar}}.
}
\examples{
substr("abcdef",2,4)
substring("abcdef",1:6,1:6)
substr(rep("abcdef",4),1:4,4:5)
x <- c("asfef", "qwerty", "yuiop[", "b", "stuff.blah.yech")
all(substr(x, 2, 5) == substring(x, 2, 5)) #> TRUE
substr(x, 2, 5)
substring(x, 2, 4:6)
}
\keyword{character}
