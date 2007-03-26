\name{remove.packages}
\alias{remove.packages}
\concept{uninstall}
\title{Remove Installed Packages}
\description{
  Removes installed packages/bundles and updates index information
  as necessary.
}
\usage{
remove.packages(pkgs, lib, version)
}
\arguments{
  \item{pkgs}{a character vector with the names of the package(s) or
    bundle(s) to be removed.}
  \item{lib}{a character vector giving the library directories to remove the
    packages from.  If missing, defaults to the first element in
    \code{\link{.libPaths}()}.}
  \item{version}{A character vector specifying version(s) with versioned
    installs of the package(s) to remove.  If none is provided,
    the system will remove an unversioned install of the package if one
    is found, otherwise the latest versioned install.}
}
\details{
  If an element of \code{pkgs} matches a bundle name, all the packages
  in the bundle will be removed.  This takes precedence over matching a
  package name.

  \code{pkgs} and \code{version} will be recycled if necessary to the
  length of the longer one.
}
\seealso{
#ifdef unix
  \code{\link{REMOVE}} for a command line version;
#endif
  \code{\link{install.packages}} for installing packages.
}
\keyword{utilities}
