\name{format}
\title{Encode in a Common Format}
\usage{
format(x, trim=FALSE)
}
\alias{format}
\value{
This function converts its first argument to a vector of character strings,
which have a common format (as is done by print).
If \code{trim} is \code{TRUE}, leading blanks are trimmed off the
strings.
This is useful when the strings are to be used for plot axis annotation.

The function \code{formatC} provides a rather more flexible
formatting facility.
}
\seealso{
\code{\link{formatC}}.
}
\examples{
format(1:10)
}
\keyword{character}
