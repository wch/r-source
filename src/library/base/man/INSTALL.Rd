\name{INSTALL}
\title{Install Add-on Packages}
\alias{INSTALL}
\usage{R INSTALL [options] [-l lib] pkgs}
\arguments{
  \item{pkgs}{A list with the path names of the packages to be
    installed.}
  \item{lib}{the path name of the \R library tree to install to.}
  \item{options}{a list of options through which in particular the build
    process for help files can be controlled.  If \code{--no-docs} is
    given, no help files are built.  \code{--no-text}, \code{--no-html},
    and \code{--no-latex} suppress creating the text, HTML, and LaTeX
    versions, respectively.  The default is to build help files in all
    three versions.}
}
\description{
  To install packages into the default library tree (which is rooted at
  \file{\$R\_HOME/library}), do \code{R INSTALL pkgs}.

  To install into the library tree \code{lib} instead of the default
  one, use \code{R INSTALL -l lib pkgs}.
}
\details{
  Both \code{lib} and the elements of \code{pkgs} may be absolute or
  relative path names. \code{pkgs} can also contain name of package
  archive files of the form \file{pkg\_version.tar.gz} as obtained from
  CRAN, these are then extracted in a temporary directory.

  For checking (via the examples in the help files \file{man/*.Rd},
  use \code{R CMD check [-l lib] <pkg>}, use \code{R CMD check --help}
  for more information.
}
\seealso{
  \code{\link{REMOVE}}, and \code{\link{library}} for information on
  using several library trees and creating packages;
  \code{\link{update.packages}} for automatic update of packages using
  the internet.
}
