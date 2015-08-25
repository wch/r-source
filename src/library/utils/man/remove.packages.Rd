% File src/library/utils/man/remove.packages.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2009 R Core Team
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
  \item{pkgs}{a character vector with the names of the packages to be removed.}
  \item{lib}{a character vector giving the library directories to remove the
    packages from.  If missing, defaults to the first element in
    \code{\link{.libPaths}()}.}
}

\seealso{
#ifdef unix
  \code{\link{REMOVE}} for a command line version;
#endif
  \code{\link{install.packages}} for installing packages.
}
\keyword{utilities}
