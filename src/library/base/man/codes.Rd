\name{codes}
\title{Factor Codes}
\usage{
codes(x)
codes(x) <- y
}
\alias{codes}
\value{
This function retrieves the underlying
integer codes used for the levels of a factor
and can also be used to reset (a subset of) these codes.
}
\seealso{
\code{\link{factor}}, \code{\link{levels}}, \code{\link{nlevels}}.
}
\examples{
x <- gl(3,5,15)
codes(x)[3] <- 2
all(unique(codes(x)) == 1:nlevels(x))#- should be true for ALL factors
}
\keyword{category}
\keyword{classes}
