\name{INSTALL}
\title{Install Add-on Packages from Sources}
\alias{INSTALL}
\usage{Rcmd INSTALL [options] [-l lib] pkgs}
\arguments{
  \item{pkgs}{A list with the path names of the sources of the
    packages to be installed.}
  \item{lib}{the path name of the \R library tree to install to.}
  \item{options}{a list of options through which in particular the build
    process for help files can be controlled.  Use \code{Rcmd INSTALL
      --help} for the current list of options.}
}
\description{
  To install packages into the default library tree (which is rooted at
  \file{\$R\_HOME/library}), do \code{Rcmd INSTALL pkgs}.

  To install into the library tree \code{lib} instead of the default
  one, use \code{Rcmd INSTALL -l lib pkgs}.
}
\details{
  Both \code{lib} and the elements of \code{pkgs} may be absolute or
  relative path names. \code{pkgs} can also contain name of package
  archive files of the form \file{pkg\_version.tar.gz} as obtained from
  CRAN (these are then extracted in a temporary directory
  \file{R.INSTALL} of the current directory).

  If the option \code{--save} is used, the installation procedure
  creates a binary image of the package code, which is then loaded when
  the library is attached, rather than evaluating the package source at
  that time.  Having a file \code{INSTALL.R} in the package directory
  (its contents don't matter) makes this the default behavior for the
  package (option \code{--no-save} overrides).  You may need \code{--save}
  if your package requires other packages to evaluate its own source.

  
  For checking (via the examples in the help files \file{man/*.Rd},
  use \code{Rcmd check [-l lib] <pkg>}: use \code{Rcmd check --help}
  for more information.
}
\seealso{
  \code{\link{REMOVE}}, and \code{\link{library}} for information on
  using several library trees and creating packages;
  \code{\link{update.packages}} for automatic update of packages using
  the internet.
}
\keyword{utilities}
