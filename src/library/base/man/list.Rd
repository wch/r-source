\name{list}
\title{Lists}
\usage{
list(\dots)
as.list(x)
is.list(x)
}
\alias{list}
\alias{as.list}
\alias{is.list}
\description{
The arguments to \code{list} are of the form \code{value}
or \code{tag=value}.
The function returns a list composed of its arguments
with each value either tagged or untagged,
depending on how the argument was specified.

\code{as.list} attempts to coerce its argument to list type.

\code{is.list} returns \code{TRUE} if its argument
is a list and \code{FALSE} otherwise.

To create an empty list it is necessary to use the
function \code{vector} with a \code{mode} of \code{"list"}.
}
\seealso{
\code{\link{vector}}.
}
\examples{
data(cars)
# create a plotting structure
pts <- list(x=cars[,1], y=cars[,2])
plot(pts)
}
\keyword{list}
\keyword{manip}
