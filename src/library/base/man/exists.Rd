\name{exists}
\title{Is an Object Defined?}
\usage{
exists(x, where = NULL, envir = sys.frame(sys.parent()),
       frame = NULL, mode = "any", inherits = TRUE)
}
\alias{exists}
\arguments{
  \item{x}{a variable name (given as a character string).}
  \item{where, envir, frame}{an environment to be searched.
    By default this is the
    environment where the call to \code{envir} takes place.}
  \item{mode}{the type of interest for the object.}
  \item{inherits}{should the enclosing frames of the environment be inspected.}
}
\description{
  Search for an \R object of the given name on the search path.
}
\details{    
  This function looks to see if the name \code{x} has a value bound to it.
  If \code{inherits} is \code{TRUE} and a value is not found for \code{x},
  then the parent frames of \code{envir} are searched until the name
  \code{x} is encountered. \bold{Warning:} This is the default behaviour
  for \R but not for S.

  If \code{mode} is specified then only objects of that mode are sought.
  The function returns \code{TRUE} if the variable is encountered and
  \code{FALSE} if not.
}
\seealso{
  \code{\link{get}}.
}
\examples{
##  Define a substitute function if necessary:
if(!exists("some.fun", mode="function"))
 some.fun <- function(x) { cat("some.fun(x)\n"); x }
search()
exists("ls", 2) # true even though ls is in pos=3
exists("ls", 2, inherits=F) # false
}
\keyword{data}
