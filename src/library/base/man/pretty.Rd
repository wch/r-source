\name{pretty}
\title{Pretty Breakpoints}
\usage{
pretty(x, n=5)
}
\alias{pretty}
\description{
\code{pretty} returns a sequence of about \code{n}
equally spaced nice values which cover the range of the values in
\code{x}.
The values are chosen so that they are 1, 2 or 5 times
a power of 10.
}
\examples{
pretty(1:15)
pretty(1:15 * 2)
pretty(1:20)
pretty(1:20, n=2)
pretty(1:20, n=10)
}
\keyword{dplot}
