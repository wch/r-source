% File src/library/utils/man/REMOVE.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Development Team
% Distributed under GPL 2 or later

\name{REMOVE}
\alias{REMOVE}
\title{Remove Add-on Packages}
\description{Utility for removing add-on packages.}
\usage{
\special{R CMD REMOVE [options] [-l lib] pkgs}
}
\arguments{
  \item{pkgs}{a space-separated list with the names of the bundles or
    packages to be removed.}
  \item{lib}{the path name of the \R library tree to remove from.  May
    be absolute or relative.  Also accepted in the form \samp{--library=lib}.}
  \item{options}{further options for help or version.}
}
\details{
  If the name of a bundle is given, the whole bundle will be removed.
  
  If used as \command{R CMD REMOVE pkgs} without explicitly specifying
  \code{lib}, packages are removed from the library tree rooted at the
  first directory in the library path which would be used by \R run in
  the current environment.

  To remove from the library tree \code{lib} instead of the default
  one, use \command{R CMD REMOVE -l lib pkgs}.

  Use \command{R CMD REMOVE --help} for more usage information.  
}
#ifdef unix
\note{
  Some binary distributions of \R have \code{REMOVE} in a separate
  bundle, e.g. an \code{R-devel} RPM.
}
#endif
\seealso{
  \code{\link{INSTALL}}, \code{\link{remove.packages}}
}
\keyword{utilities}
