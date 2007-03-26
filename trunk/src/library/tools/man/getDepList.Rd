\name{getDepList}
\alias{getDepList}
\alias{pkgDepends}
\title{Functions to Retrieve Dependency Information}
\description{
  Given a dependency matrix, will create a \code{DependsList} object for that
  package which will include the dependencies for that matrix, which
  ones are installed, which unresolved dependencies were found online,
  which unresolved dependencies were not found online, and any R dependencies.
}
\usage{
getDepList(depMtrx, instPkgs, recursive = TRUE, local = TRUE,
           reduce = TRUE, lib.loc = NULL)

pkgDepends(pkg, recursive = TRUE, local = TRUE, reduce = TRUE,
           lib.loc = NULL)
}
\arguments{
  \item{depMtrx}{A dependency matrix as from \code{package.dependencies}}
  \item{pkg}{The name of the package}
  \item{instPkgs}{A matrix specifying all packages installed on the
    local system, as from \code{installed.packages}}
  \item{recursive}{Whether or not to include indirect dependencies}
  \item{local}{Whether or not to search only locally}
  \item{reduce}{Whether or not to collapse all sets of dependencies to a
    minimal value}
  \item{lib.loc}{What libraries to use when looking for installed
    packages.  \code{NULL} indicates all library directories in the
    user's \code{.libPaths()}.}
}
\details{
  The function \code{pkgDepends} is a convenience function which wraps
  \code{getDepList} and takes as input a package name.  It will then
  query \code{\link{installed.packages}}
  and also generate a dependency
  matrix, calling \code{getDepList} with this information and returning
  the result.
  
  These functions will retrieve information about the dependencies of
  the matrix, resulting in a \code{DependsList} object.  This is a
  list with four elements:
  \describe{
    \item{Depends}{A vector of the dependencies for this package.}
    \item{Installed}{A vector of the dependencies which have been
      satisfied by the currently installed packages.}
    \item{Found}{A list representing the dependencies which are not in
      \code{Installed} but were found online.  This list has element
      names which are the URLs for the repositories in which packages
      were found and the elements themselves are vectors of package
      names which were found in the respective repositories.  If
      \code{local=TRUE}, the \code{Found} element will always be empty.}
    \item{R}{Any R version dependencies.}
  }
    
  If \code{recursive} is \code{TRUE}, any package that is specified as a
  dependency will in turn have its dependencies included (and so on),
  these are known as indirect dependencies.  If \code{recursive} is 
  \code{FALSE}, only the dependencies directly stated by the package will 
  be used.

  If \code{local} is \code{TRUE}, the system will only look at the user's local
  install and not online to find unresolved dependencies.

  If \code{reduce} is \code{TRUE}, the system will collapse the fields in the
  \code{DependsList} object such that a minimal set of dependencies
  are specified (for instance if there was ('foo', 'foo (>= 1.0.0)',
  'foo (>= 1.3.0)'), it would only return 'foo (>= 1.3.0)').
}
\value{
  An object of class \code{DependsList}
}
\author{ Jeff Gentry }
\seealso{\code{\link{installFoundDepends}}}
\examples{
pkgDepends("tools", local = FALSE)
}
\keyword{utilities}

