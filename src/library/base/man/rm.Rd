\name{remove}
\title{Remove Objects from a Specified Environment}
\usage{
remove(\dots, list = character(0), pos = -1,
       envir = as.environment(pos), inherits = FALSE)

rm    (\dots, list = character(0), pos = -1,
       envir = as.environment(pos), inherits = FALSE)
}
\alias{rm}
\alias{remove}
\arguments{
  \item{\dots}{the objects to be removed, as names (unquoted) or
    character strings (quoted).}
  \item{list}{a character vector naming objects to be removed.}
  \item{pos}{where to do the removal.  By default, uses the
    current environment.  See the details for other possibilities.}
  \item{envir}{the \code{\link{environment}} to use. See the details section.}
  \item{inherits}{should the enclosing frames of the environment be
    inspected?}
}
\description{
  \code{remove} and \code{rm} can be used to remove objects.  These can
  be specified successively as character strings, or in the character
  vector \code{list}, or through a combination of both.  All objects
  thus specified will be removed.

  If \code{envir} is NULL then the currently active environment is
  searched first.

  If \code{inherits} is \code{TRUE} then parents of the supplied
  directory are searched until a variable with the given name is
  encountered.  A warning is printed for each variable that is not
  found.
}
\details{
  The \code{pos} argument can specify the  environment from which to remove
  the objects in any of several ways:
  as an integer (the position in the \code{\link{search}} list); as
  the character string name of an element in the search list; or as an
  \code{\link{environment}} (including using \code{\link{sys.frame}} to
  access the currently active function calls).
  The \code{envir} argument is an alternative way to specify an
  environment, but is primarily there for back compatibility.

  It is not allowed to remove variables from the base environment and
  base name space, nor from any environment which is locked (see
  \code{\link{lockEnvironment}}).

  Earlier versions of \R incorrectly claimed that supplying a character
  vector in \code{\dots} removed the objects named in the character
  vector, but it removed the character vector.  Use the \code{list}
  argument to specify objects \emph{via} a character vector.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
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
