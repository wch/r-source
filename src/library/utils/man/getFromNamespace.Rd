% File src/library/utils/man/getFromNamespace.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{getFromNamespace}
\alias{assignInNamespace}
\alias{getFromNamespace}
\alias{fixInNamespace}
\title{Utility functions for Developing Namespaces}
\description{
  Utility functions to access and replace the non-exported functions in
  a name space, for use in developing packages with name spaces.
}
\usage{
getFromNamespace(x, ns, pos = -1, envir = as.environment(pos))

assignInNamespace(x, value, ns, pos = -1,
                  envir = as.environment(pos))

fixInNamespace(x, ns, pos = -1, envir = as.environment(pos), \dots)
}
\arguments{
  \item{x}{an object name (given as a character string).}
  \item{value}{an \R object.}
  \item{ns}{a name space, or character string giving the name space.}
  \item{pos}{where to look for the object: see \code{\link{get}}.}
  \item{envir}{an alternative way to specify an environment to look in.}
  \item{\dots}{arguments to pass to the editor: see \code{\link{edit}}.}
}
\details{
  The name space can be specified in several ways.  Using, for example,
  \code{ns = "stats"} is the most direct, but a loaded package with a
  name space can be specified via any of the methods used for
  \code{\link{get}}: \code{ns} can also be the environment printed as
  \code{<namespace:foo>}.

  \code{getFromNamespace} is similar to (but predates) the
  \code{\link{:::}} operator, but is more flexible in how the name space
  is specified.
  
  \code{fixInNamespace} invokes \code{\link{edit}} on the object named
  \code{x} and assigns the revised object in place of the original
  object.  For compatibility with \code{fix}, \code{x} can be unquoted.
}
\note{
  \code{assignInNamespace} and \code{fixInNamespace} change the copy in
  the name space, but not any copies already exported from the name space,
  in particular an object of that name in the package (if already
  attached) and any copies already imported into other name spaces.
  They are really intended to be used \emph{only} for objects which are
  not exported from the name space.  They do attempt to alter a copy
  registered as an S3 method if one is found.

  They can only be used to change the values of objects in the
  name space, not to create new objects.
}
\value{
  \code{getFromNamespace} returns the object found (or gives an error).

  \code{assignInNamespace} and \code{fixInNamespace} are invoked for
  their side effect of changing the object in the name space.
}

\seealso{
  \code{\link{get}}, \code{\link{fix}}, \code{\link{getS3method}}
}
\examples{
getFromNamespace("findGeneric", "utils")
\dontrun{
fixInNamespace("predict.ppr", "stats")
stats:::predict.ppr
getS3method("predict", "ppr")
## alternatively
fixInNamespace("predict.ppr", pos = 3)
fixInNamespace("predict.ppr", pos = "package:stats")
}}
\keyword{data}
