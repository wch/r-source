\name{REMOVE}
\title{Remove Add-on Packages}
\alias{REMOVE}
\usage{R REMOVE [-l lib] pkgs}
\arguments{
  \item{pkgs}{a list with the names of the packages to be removed.}
  \item{lib}{the path name of the \R library tree to remove from.  May
    be absolute or relative.} 
}
\description{
  Use \code{R REMOVE pkg} to remove the package \code{pkg} from the
  default library tree (which is rooted at \file{\$RHOME/library}).

  To remove from the library tree \code{lib} instead of the default
  one, use \code{R REMOVE -l lib pkg}.
}
\seealso{
  \code{\link{INSTALL}}
}
