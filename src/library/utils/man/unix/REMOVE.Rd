\name{REMOVE}
\alias{REMOVE}
\title{Remove Add-on Packages}
\description{Utility for removing add-on packages.}
\usage{R CMD REMOVE [options] [-l lib] pkgs}
\arguments{
  \item{pkgs}{a list with the names of the packages to be removed.}
  \item{lib}{the path name of the \R library tree to remove from.  May
    be absolute or relative.}
  \item{options}{further options.}
}
\details{
  If used as \code{R CMD REMOVE pkgs} without explicitly specifying
  \code{lib}, packages are removed from the library tree rooted at the
  first directory given in the environment variable \env{R\_LIBS} if
  this is set and non-null, and to the default library tree (which is
  rooted at \file{\$R\_HOME/library}) otherwise.

  To remove from the library tree \code{lib}, use
  \code{R CMD REMOVE -l lib pkgs}.

  Use \code{R CMD REMOVE --help} for more usage information.  
}
\note{
  Some binary distributions of \R have \code{INSTALL} in a separate
  bundle, e.g. an \code{R-devel} RPM.
}
\seealso{
  \code{\link{INSTALL}}
}
\keyword{utilities}
