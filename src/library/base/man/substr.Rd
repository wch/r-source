\name{substr}
\alias{substr}
\alias{substring}
\alias{substr<-}
\alias{substring<-}
\title{Substrings of a Character Vector}
\usage{
substr(x, start, stop)
substring(text, first, last = 1000000)
substr(x, start, stop) <- value
substring(text, first, last = 1000000) <- value
}
\arguments{
  \item{x, text}{a character vector}
  \item{start, first}{integer. The first element to be replaced.}
  \item{stop, last}{integer. The last element to be replaced.}
  \item{value}{a character vector, recycled if necessary.}
}
\description{
  Extract or replace substrings in a character vector.
}
\value{
  For \code{substr}, a character vector of the same length as \code{x}.

  For \code{substring}, a character vector of length the longest of the
  arguments.
}
\details{
  \code{substring} is compatible with S, with \code{first} and
  \code{last} instead of \code{start} and \code{stop}.
  For vector arguments, it expands the arguments cyclically to the
  length of the longest.

  When extracting, if \code{start} is larger than the string length then
  \code{""} is returned.

  For the replacement functions, if \code{start} is larger than the
  string length then no replacement is done.  If the portion to be
  replaced is longer than the replacement string, then only the
  portion the length of the string is replaced.
}
\note{
  The S4 version of \code{substring<-} ignores \code{last}; this version
  does not.
}
\seealso{
 \code{\link{strsplit}}, \code{\link{paste}}, \code{\link{nchar}}.
}
\examples{
substr("abcdef",2,4)
substring("abcdef",1:6,1:6)
## strsplit is more efficient ...

substr(rep("abcdef",4),1:4,4:5)
x <- c("asfef", "qwerty", "yuiop[", "b", "stuff.blah.yech")
substr(x, 2, 5)
substring(x, 2, 4:6)

substring(x, 2) <- c("..", "+++")
x
}
\keyword{character}
