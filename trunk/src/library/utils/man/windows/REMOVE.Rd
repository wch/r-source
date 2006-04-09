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
  Use \code{R CMD REMOVE pkgs} to remove the packages in \code{pkgs} from
  the default library tree (which is rooted at
  \file{\$R\_HOME/library}).

  To remove from the library tree \code{lib} instead of the default
  one, use \code{R CMD REMOVE -l lib pkgs}.
}
\details{
  Windows Perl and the files for installing from source packages need to
  be installed.  
}
\seealso{
  \code{\link{INSTALL}}
}
\keyword{utilities}
