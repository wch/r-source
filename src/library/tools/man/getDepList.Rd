% File src/library/tools/man/getDepList.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{getDepList}
\alias{getDepList}
\alias{pkgDepends}
\title{Functions to Retrieve Dependency Information}
\description{
  These functions retrieve information about the dependencies of
  a package or a dependency matrix, resulting in a \code{DependsList} object.

  Given a dependency matrix, \code{getDepList()} creates a
  \code{DependsList} object for that package.  That object includes the
  dependencies for that matrix, which ones are installed, which
  unresolved dependencies were found online, which unresolved
  dependencies were not found online, and any R dependencies.

  \code{pkgDepends()} for convenience takes a package name, builds a
  dependency matrix and then returns a corresponding \code{getDepList()}.
}
\usage{
getDepList(depMtrx, instPkgs, recursive = TRUE, local = TRUE,
           reduce = TRUE, lib.loc = NULL)

pkgDepends(pkg, recursive = TRUE, local = TRUE, reduce = TRUE,
           lib.loc = NULL)
}
\arguments{
  \item{depMtrx}{a dependency matrix as from \code{\link{package.dependencies}()}.}
  \item{pkg}{the name of the package}
  \item{instPkgs}{a matrix specifying all packages installed on the
    local system, as from \code{installed.packages}}
  \item{recursive}{whether or not to include indirect dependencies.}
  \item{local}{whether or not to search only locally}
  \item{reduce}{whether or not to collapse all sets of dependencies to a
    minimal value}
  \item{lib.loc}{what libraries to use when looking for installed
    packages.  \code{NULL} indicates all library directories in the
    current \code{.libPaths()}.
    Note that \code{lib.loc} is not used in \code{getDepList()} and
    deprecated there.}
}
\details{
  The convenience function \code{pkgDepends(pkg)} queries
  \code{\link{installed.packages}} and generates a dependency
  matrix, calling \code{getDepList} with this information and returning
  the result.

  If \code{recursive} is \code{TRUE}, any package that is specified as a
  dependency will in turn have its dependencies included (and so on),
  these are known as indirect dependencies.  If \code{recursive} is
  \code{FALSE}, only the dependencies directly stated by the package will
  be used.

  If \code{local} is \code{TRUE}, the system will only look at the user's local
  install and not online to find unresolved dependencies.

  If \code{reduce} is \code{TRUE}, the system will collapse the fields in the
  \code{DependsList} object such that a minimal set of dependencies
  are specified (for instance if there was
  \samp{foo, foo (>= 1.0.0), foo (>= 1.3.0)}, it would only return
  \samp{foo (>= 1.3.0)}).
}
\value{
  An object of S3 class \code{"DependsList"}, which is a \code{\link{list}}
  with components
  \item{Depends}{A vector of the dependencies for this package.}
  \item{Installed}{A vector of the dependencies which have been
    satisfied by the currently installed packages.}
  \item{Found}{A list representing the dependencies which are not in
    \code{Installed} but were found online.  This list has element
    names which are the URLs for the repositories in which packages
    were found and the elements themselves are vectors of package
    names which were found in the respective repositories.  If
    \code{local = TRUE}, the \code{Found} element will always be empty.}
  \item{NotFound}{...}
  \item{R}{Any R version dependencies.}
}
\author{ Jeff Gentry }
\seealso{\code{\link{installFoundDepends}}}
\examples{
pkgDepends("tools", local = FALSE)
}
\keyword{utilities}

