\name{list}
\title{Lists}
\usage{
list(\dots)
as.list(x)
is.list(x)
alist(\dots)
}
\alias{list}
\alias{alist}
\alias{as.list}
\alias{is.list}
\description{
The arguments to \code{list} are of the form \code{value}
or \code{tag=value}.
The function returns a list composed of its arguments
with each value either tagged or untagged,
depending on how the argument was specified.

\code{alist} is like \code{list}, except in the handling of tagged
arguments with no value. These are handled as if they described
function arguments with no default (cf. \code{\link{formals}}), whereas
\code{list} simply ignores them.

\code{as.list} attempts to coerce its argument to list type.
For functions, this returns the concatenation of the list of formals
arguments and the function body. For expressions, the list of
constituent calls is returned.

\code{is.list} returns \code{TRUE} if its argument
is a list and \code{FALSE} otherwise.

To create an empty list, it is necessary to use the
function \code{vector} with a \code{\link{mode}} of \code{"list"}.
}
\seealso{
\code{\link{vector}(., mode="list")}, \code{\link{c}}, for concatenation;
\code{\link{formals}}.
}
\examples{
data(cars)
# create a plotting structure
pts <- list(x=cars[,1], y=cars[,2])
plot(pts)

# Argument lists
f <- function()x
formals(f) <- alist(x=, y=2, ...=) # Note the spec. of a "..." argument
f
}
\keyword{list}
\keyword{manip}
