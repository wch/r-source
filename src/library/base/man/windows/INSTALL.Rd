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
  Windows Perl and the files for installing from source packages need to
  be installed.
  
  Both \code{lib} and the elements of \code{pkgs} may be absolute or
  relative path names. \code{pkgs} can also contain name of package
  archive files of the form \file{pkg\_version.tar.gz} as obtained from
  CRAN (these are then extracted in a temporary directory
  \file{R.INSTALL} of the current directory).

  If the option \option{--save} is used, the installation procedure
  creates a binary image of the package code, which is then loaded when
  the library is attached, rather than evaluating the package source at
  that time.  Having a file \code{install.R} in the package directory
  makes this the default behavior for the package (option
  \option{--no-save} overrides).  You may need \option{--save} if your
  package requires other packages to evaluate its own source.  If the
  file \file{install.R} is non-empty, it should contain R expressions to
  be executed when the package is attached, after loading the saved
  image.  

  For checking (via the examples in the help files \file{man/*.Rd},
  use \code{Rcmd check [-l lib] <pkg>}: use \code{Rcmd check --help}
  for more information.

  Which forms of help are produced is controlled by the \option{--docs}
  option which takes values \code{none} or \code{normal} (text and HTML)
  or \code{chm} (the default, text, HTML and CHTML) or \code{winhlp} or
  \code{all} (adding old-style Windows \code{.hlp} files, no longer tested).
}

\section{Packages using the methods package}{
Packages that require the methods package, and that use functions such as
\code{\link[methods]{setMethod}} or \code{\link[methods]{setClass}},
should be installed by creating a binary image.

  The presence of a file named
  \file{install.R} in the package's main directory causes an image to
  be saved.  Note that the file is not in the \file{R} subdirectory: all
  the code in that subdirectory is used to construct the binary image.

  Normally, the file \file{install.R} will be empty; if it does contain R expressions
  these will be evaluated when the package is attached, e.g. by a call to
  the function \code{\link{library}}.  (Specifically, the source code
  evaluated for a package with a saved image consists of a suitable
  definition of \code{.First.lib} to ensure loading of the saved
  image, followed by the R code in file \file{install.R}, if any.)

}
\seealso{
  \code{\link{REMOVE}}, and \code{\link{library}} for information on
  using several library trees and creating packages;
  \code{\link{update.packages}} for automatic update of packages using
  the internet.
}
\keyword{utilities}
