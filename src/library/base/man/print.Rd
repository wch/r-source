\name{print}
\title{Print Values}
\usage{
print(x, \dots)
}
\alias{print}
\description{
\code{print} prints its argument.
It is implemented as a generic function which
means that new printing methods can be easily
added for new classes.

The default version of print is described in \code{print.default}.
}
\seealso{
\code{\link{print.default}}.
}
\examples{
ts(1:20)#-- print  is the ``Default function'' --
rr <- for(i in 1:3) print(1:i)
rr
}
\keyword{print}
