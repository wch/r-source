\name{remove}
\title{Remove Objects from a Specified Environment}
\usage{
remove(\dots, list = character(0), pos = -1, envir = pos.to.env(pos),
       inherits = FALSE)
rm    (\dots, list = character(0), pos = -1, envir = pos.to.env(pos),
       inherits = FALSE)
}
\alias{rm}
\alias{remove}
\description{
  \code{remove} and \code{rm} can be used to remove objects.  These can
  be specified successively as character strings, or in the character
  vector \code{list}, or through a combination of both.  All objects
  thus specified will be removed.

  If \code{envir} is NULL then the the currently active environment is
  searched first.

  If \code{inherits} is \code{TRUE} then parents of the supplied
  directory are searched until a variable with the given name is
  encountered.  A warning is printed for each variable that is not
  found.
}
\seealso{
  \code{\link{ls}}, \code{\link{objects}}
}
\examples{
tmp <- 1:4
## work with tmp  and cleanup
rm(tmp)

\dontrun{
## remove (almost) everything in the working environment.
## You will get no warning, so don't do this unless you are really sure.
rm(list = ls())
}}
\keyword{environment}
