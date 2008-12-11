% File src/library/utils/man/remove.packages.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Development Team
% Distributed under GPL 2 or later

\name{remove.packages}
\alias{remove.packages}
\concept{uninstall}
\title{Remove Installed Packages}
\description{
  Removes installed packages/bundles and updates index information
  as necessary.
}
\usage{
remove.packages(pkgs, lib)
}
\arguments{
  \item{pkgs}{a character vector with the names of the package(s) or
    bundle(s) to be removed.}
  \item{lib}{a character vector giving the library directories to remove the
    packages from.  If missing, defaults to the first element in
    \code{\link{.libPaths}()}.}
}
\details{
  If an element of \code{pkgs} matches a bundle name, all the packages
  in the bundle will be removed.  This takes precedence over matching a
  package name.
}
\seealso{
#ifdef unix
  \code{\link{REMOVE}} for a command line version;
#endif
  \code{\link{install.packages}} for installing packages.
}
\keyword{utilities}
