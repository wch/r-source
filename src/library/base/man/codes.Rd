\name{codes}
\title{Factor Codes}
\usage{
codes(x)
codes(x) <- value
}
\alias{codes}
\alias{codes.factor}
\alias{codes<-}
\value{
This (generic) function retrieves the underlying
integer codes used for the levels of a factor
and can also be used to reset (a subset of) these codes.
}
\seealso{
\code{\link{factor}}, \code{\link{levels}}, \code{\link{nlevels}}.
}
\examples{
x <- gl(3,5)
codes(x)[3] <- 2
all(unique(codes(x)) == 1:nlevels(x))#- should be true for ALL factors
}
\keyword{category}
\keyword{classes}
