\name{substr}
\title{Extract Substrings from a Character Vector}
\usage{
substr(x, start, stop)
substring(text, first, last = 1000000)
}
\alias{substr}
\alias{substring}
\description{Extract substrings from a character vector returning a
    vector whose elements contain the substring starting with the
    character at position \code{start} up to the character at position
    \code{stop}.
}
\details{
    If \code{start} is larger than the string length then
    \code{\link{NA}} is returned.
    If \code{stop} is longer than \code{start} an error is signalled.

    \code{substring} is compatible with S, with \code{first} and
    \code{last} instead of \code{start} and \code{stop}.
    For vector arguments, it expands the arguments cyclically.
}
\seealso{
 \code{\link{strsplit}}, \code{\link{paste}}, \code{\link{nchar}}.
}
\examples{
substr("abcdef",2,4)
print(ss <- substring("abcdef",1:6,1:6))
stopifnot(ss == strsplit ("abcdef",NULL)[[1]])# strsplit is more efficient..

substr(rep("abcdef",4),1:4,4:5)
x <- c("asfef", "qwerty", "yuiop[", "b", "stuff.blah.yech")
stopifnot(substr(x, 2, 5) == substring(x, 2, 5))
substr(x, 2, 5)
substring(x, 2, 4:6)
}
\keyword{character}
