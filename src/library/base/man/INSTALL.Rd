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
  To install a package into the default library tree (which is rooted at
  \file{\$RHOME/library}), do \code{R INSTALL pkg}.

  To install into the library tree \code{lib} instead of the default
  one, use \code{R INSTALL -l lib pkg}.

  Both \code{lib} and the elements of \code{pkgs} may be absolute or
  relative path names.
}
\seealso{
  \code{\link{REMOVE}}, and \code{\link{library}} for information on
  using several library trees and creating packages.
}
