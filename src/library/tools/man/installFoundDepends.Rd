\name{installFoundDepends}
\alias{installFoundDepends}
\title{A function to install unresolved dependencies}
\description{
  This function will take the \code{Found} element of a
  \code{pkgDependsList} object and attempt to install 
  all of the listed packages from the specified repositories.
}
\usage{
installFoundDepends(depPkgList, ...)
}
\arguments{
  \item{depPkgList}{A \code{Found} element from a \code{pkgDependsList} object}
  \item{\dots}{Arguments to pass on to \code{\link{install.packages}}}
}
\details{
  This function takes as input the \code{Found} list from a
  \code{pkgDependsList} object.  This list will have element names being
  URLs corresponding to repositories and the elements will be vectors of
  package names.  For each element, \code{\link{install.packages}} is
  called for that URL to install all packages listed in the vector.
}
\author{Jeff Gentry}
\seealso{\code{\link{pkgDepends}}, \code{\link{install.packages}}}
\examples{
## Set up a temporary directory to install packages to
tmp <- tempfile()
dir.create(tmp)

pDL <- pkgDepends("tools",local=FALSE)
installFoundDepends(pDL$Found, destdir=tmp)
}
\keyword{utilities}
