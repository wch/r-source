\name{remove}
\title{Remove Objects from a Specified Environment}
\usage{
remove(names, envir=NULL, inherits=FALSE)
rm(names, envir=NULL, inherits=FALSE)
}
\alias{rm}
\alias{remove}
\description{
\code{remove} and \code{rm} can be used to remove objects.
\code{names} must be character vector.
All elements in it will be removed.
If \code{envir} is NULL then the the currently active
environment is searched first.
If \code{inherits} is \code{TRUE}
then parents of the supplied directory are searched until a
variable with the given name is encountered. 
A warning is printed for each variable that is not found.
}
\seealso{
\code{\link{ls}},
\code{\link{objects}},
}
