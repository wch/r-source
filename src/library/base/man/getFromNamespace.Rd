\name{getFromNamespace}
\alias{getFromNamespace}
\alias{fixInNamespace}
\title{Utility functions for Developing Namespaces}
\description{
  Utility functions to access and replace the non-exported functions in
  a namespace, for use in developing packages with namespaces.
}
\usage{
getFromNamespace(x, ns, pos = -1, envir = as.environment(pos))
fixInNamespace(x, ns, pos = -1, envir = as.environment(pos), \dots)
}
\arguments{
  \item{x}{an object name (given as a character string).}
  \item{ns}{a namespace, or character string giving the namespace.}
  \item{pos}{where to look for the object: see \code{\link{get}}.}
  \item{envir}{an alternative way to specify an environment to look in.}
  \item{\dots}{arguments to pass to the editor: see \code{\link{edit}}.}
}
\details{
  The namespace can be specified in several ways.  Using, for example,
  \code{ns="modreg"} is the most direct, but a loaded package with a
  namespace can be specified via any of the methods used for
  \code{\link{get}}: \code{ns} can also be the environment
  \code{<namespace:foo>}.

  \code{fixInNamespace} invokes \code{\link{edit}} on the object named
  \code{x} and assigns the revised object in place of the original
  object.  For compatibility with \code{fix}, \code{x} can be unquoted.
}
\note{
  \code{fixInNamespace} will alter the copy of the object in the
  namespace, and also a copy registered as an S3 method.  There can be other
  copies, so the function is not foolproof, but should be helpful for
  debugging.
}
\value{
  \code{getFromNamespace} returns the object found (or gives an error).

  \code{fixInNamespace} is invoked for its side effect of changing the
  object in the namespace.
}

\seealso{
  \code{\link{get}}, \code{\link{fix}}, \code{\link{getS3method}}
}
\examples{\dontrun{
fixInNamespace("predict.ppr", "modreg")
## alternatively
fixInNamespace("predict.ppr", pos = 5)
}}
\keyword{data}
