\name{REMOVE}
\title{Remove Add-on Packages}
\alias{REMOVE}
\usage{
\special{R CMD REMOVE [-l lib] pkgs}
}
\arguments{
  \item{pkgs}{a list with the names of the packages to be removed.}
  \item{lib}{the path name of the \R library tree to remove from.  May
    be absolute or relative.} 
}
\description{
  Utility to remove add-on packages.
}
\details{
  If used as \code{R CMD REMOVE pkgs} without explicitly specifying
  \code{lib}, packages are removed from the library tree rooted at the
  first directory in the library path which would be used by \R run in
  the current environment.

  To remove from the library tree \code{lib} instead of the default
  one, use \code{R CMD REMOVE -l lib pkgs}.

  Windows Perl and the files for installing from source packages need to
  be installed.  
}
\seealso{
  \code{\link{INSTALL}}, \code{\link{remove.packages}}
}
\keyword{utilities}
