\name{nchar}
\title{Count the Number of Characters}
\usage{nchar(x)}
\alias{nchar}
\description{
\code{nchar} takes a character vector as an argument and
returns a vector whose elements contain the number of characters in
the corresponding element of \code{x}.
It only accepts character vectors as arguments if you want to operate
on other objects passing them through \code{deparse} first will be required.
}
\arguments{
  \item{x}{character vector.} 
}
\seealso{
\code{\link{strwidth}} giving width of strings for plotting;
\code{\link{paste}}, \code{\link{substr}}, \code{\link{strsplit}}
}
\examples{
x<-c("asfef","qwerty","yuiop[","b","stuff.blah.yech")
nchar(x)
# 5  6  6  1 15

nchar(deparse(mean))
# 23  1 16 45 11 64  2 17 50 43  2 17  1
}
\keyword{character}
