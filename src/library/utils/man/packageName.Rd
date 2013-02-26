\name{packageName}
\alias{packageName}
\title{
Find package associated with an environment.
}
\description{
Many environments are associated with a package; this function
attempts to determine that package.
}
\usage{
packageName(env = parent.frame())
}
\arguments{
  \item{env}{
The environment whose name we seek.
}
}
\details{

Environment \code{env} would be associated with a package if
\code{\link{topenv}(env)} is the namespace environment for that
package.  Thus when \code{env} is the environment associated with
functions inside a package, or local functions defined within them,
\code{packageName} will normally return the package name.

Not all environments are associated with a package:  for example, 
the global environment, or the evaluation frames of functions defined
there.  \code{packageName} will return \code{NULL} in these cases.
}
\value{
A length one character vector containing the name of the package,
or \code{NULL} if there is no name.
}
\seealso{
\code{\link[methods]{getPackageName}} is a more elaborate function
that can construct a name if none is found.
}
\examples{
packageName()
packageName(environment(mean))
}
\keyword{ utilities }
