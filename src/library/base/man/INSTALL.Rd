\name{INSTALL}
\title{Install Add-on Packages}
\alias{INSTALL}
\usage{R INSTALL pkg [lib]}
\arguments{
  \item{pkg}{the path name to the package to be installed.}
  \item{lib}{the path name of the \R library tree to install to.}
}
\description{
  To install a package into the default library tree (which is rooted at
  \file{\$RHOME/library}), do \code{R INSTALL pkg}.

  To install into the library tree \code{lib} instead of the default
  one, use \code{R INSTALL pkg lib}.

  Both \code{pkg} and \pkg{lib} may be absolute or relative path names.
}
\seealso{
  \code{\link{REMOVE}}, and \code{\link{library}} for information on
  using several library trees and creating packages.
}
