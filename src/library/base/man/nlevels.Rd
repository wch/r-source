\name{nlevels}
\title{The Number of Levels of a Factor}
\usage{
nlevels(x)
}
\alias{nlevels}
\description{
\code{nlevels}
returns the number of levels which its argument has.
If the argument is not a \code{\link{factor}}, \code{NA} is returned.

The actual factor levels (if they exist) can be obtained
with the \code{\link{levels}} function.
}
\examples{
nlevels(gl(3,7)) # = 3
}
\keyword{category}
