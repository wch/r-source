\name{remove.packages}
\alias{remove.packages}
\title{Remove Installed Packages}
\description{
  Removes installed packages and updates index information as necessary.
}
\usage{
remove.packages(pkgs, lib, version)
}
\arguments{
  \item{pkgs}{a character vector with the names of the packages to be
    removed.}
  \item{lib}{a character string giving the library directory to move the
    packages from.}
  \item{version}{A character string specifying a specific version of the
    package to remove.  If none is provided, the system will remove an
    unversioned install of the package.}
}
\seealso{
#ifdef unix
  \code{\link{REMOVE}} for a command line version;
#endif
  \code{\link{install.packages}} for installing packages.
}
\keyword{utilities}
