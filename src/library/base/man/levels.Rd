\name{levels}
\title{Level Names for a Factor}
\usage{
levels(x)
levels(x) <- value
}
\alias{levels}
\alias{levels<-}
\description{
\code{levels} provides access to the levels attribute of factor.
The first form prints the levels of the observations
and the second sets the levels.
In this case,
\code{value} must be a vector of character strings with
length equal to the number of levels of \code{x}.

To find out how many levels a factor has user the function
\code{nlevels}.
}
\seealso{
\code{\link{factor}}, \code{\link{nlevels}}.
}
\keyword{category}
