% File src/library/utils/man/getFromNamespace.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{getFromNamespace}
\alias{assignInNamespace}
\alias{assignInMyNamespace}
\alias{getFromNamespace}
\alias{fixInNamespace}
\title{Utility Functions for Developing Namespaces}
\description{
  Utility functions to access and replace the non-exported functions in
  a namespace, for use in developing packages with namespaces.

  They should not be used in production code (except perhaps
  \code{assignInMyNamespace}, but see the \sQuote{Note}).
}
\usage{
getFromNamespace(x, ns, pos = -1, envir = as.environment(pos))

assignInNamespace(x, value, ns, pos = -1,
                  envir = as.environment(pos))

assignInMyNamespace(x, value)

fixInNamespace(x, ns, pos = -1, envir = as.environment(pos), \dots)
}
\arguments{
  \item{x}{an object name (given as a character string).}
  \item{value}{an \R object.}
  \item{ns}{a namespace, or character string giving the namespace.}
  \item{pos}{where to look for the object: see \code{\link{get}}.}
  \item{envir}{an alternative way to specify an environment to look in.}
  \item{\dots}{arguments to pass to the editor: see \code{\link{edit}}.}
}
\details{
  \code{assignInMyNamespace} is intended to be called from functions
  within a package, and chooses the namespace as the environment of the
  function calling it.

  The namespace can be specified in several ways.  Using, for example,
  \code{ns = "stats"} is the most direct, but a loaded package can be
  specified via any of the methods used for \code{\link{get}}: \code{ns}
  can also be the environment printed as \samp{<namespace:foo>}.

  \code{getFromNamespace} is similar to (but predates) the
  \code{\link{:::}} operator: it is more flexible in how the namespace
  is specified.

  \code{fixInNamespace} invokes \code{\link{edit}} on the object named
  \code{x} and assigns the revised object in place of the original
  object.  For compatibility with \code{fix}, \code{x} can be unquoted.
}
\section{Warning}{
  \code{assignInNamespace} should not be used in final code, and will in
  future throw an error if called from a package.  Already certain uses
  are disallowed.
}
\note{
  \code{assignInNamespace}, \code{assignInMyNamespace} and
  \code{fixInNamespace} change the copy in the namespace, but not any
  copies already exported from the namespace, in particular an object of
  that name in the package (if already attached) and any copies already
  imported into other namespaces.  They are really intended to be used
  \emph{only} for objects which are not exported from the namespace.
  They do attempt to alter a copy registered as an S3 method if one is
  found.

  They can only be used to change the values of objects in the
  namespace, not to create new objects.
}
\value{
  \code{getFromNamespace} returns the object found (or gives an error).

  \code{assignInNamespace}, \code{assignInMyNamespace} and
  \code{fixInNamespace} are invoked for their side effect of changing
  the object in the namespace.
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
