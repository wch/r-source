\name{remove.packages}
\alias{remove.packages}
\title{Remove Installed Packages}
\description{
  Removes installed packages and updates index information as necessary.
}
\usage{
remove.packages(pkgs, lib)
}
\arguments{
  \item{pkgs}{a character vector with the names of the packages to be
    removed.}
  \item{lib}{a character string giving the library directory to move the
    packages from.}
}
\seealso{
#ifdef unix
  \code{\link{REMOVE}} for a command line version;
#endif
  \code{\link{install.packages}} for installing packages.
}
\keyword{utilities}
