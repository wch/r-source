\name{format}
\title{Encode in a Common Format}
\usage{
format(x, \dots)
format.default(x, trim=FALSE, digits = .Options$digits)
}
\alias{format}
\alias{format.default}
\arguments{
  \item{x}{any \R object (conceptually); typically numeric.}
  \item{trim}{logical; if \code{TRUE}, leading blanks are trimmed off the
    strings.}
  \item{digits}{how many significant digits are to be used for
    \code{\link{numeric} x}.}
}
\description{
This function converts its first argument to a vector of character strings,
which have a common format (as is done by \code{\link{print}}).
The trimming with \code{trim = TRUE} is useful when the strings are to
be used for plot axis annotation.

The function \code{\link{formatC}} provides a rather more flexible
formatting facility for numbers.
}
\seealso{
\code{\link{formatC}}, \code{\link{paste}}, \code{\link{as.character}}.
}
\examples{
format(1:10)
for(i in c(1:5,10,15:17,20)) cat(i,":\t",format(pi,digits=i),"\n")
}
\keyword{character}
\keyword{print}
