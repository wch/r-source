\name{strsplit}
\title{Split the Strings in a Vector}
\usage{
strsplit(x,split)
}
\alias{strsplit}
\description{
\code{strsplit} takes two arguments. A character vector to split, \code{x}, and a
character string, \code{split}, to use as splits (this can also be a vector).
It returns a list each element of which contains the vector of splits.
}
\seealso{
\code{\link{paste}}, \code{\link{nchar}}, \code{\link{substr}}
}
\examples{
x <- c("asfef","qwerty","yuiop[","b","stuff.blah.yech")
#split x on the letter e
strsplit(x,"e")
}
\keyword{character}
