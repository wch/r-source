\name{INSTALL}
\title{Install Add-on Packages from Sources}
\alias{INSTALL}
\usage{
\special{R CMD INSTALL [options] [-l lib] pkgs}
}
\arguments{
  \item{pkgs}{A space-separated list with the path names of the sources of the
    packages to be installed.}
  \item{lib}{the path name of the \R library tree to install to.}
  \item{options}{a space-separated list of options through which in
    particular the process for building the help files can be controlled.
    Options should only be given once, and paths including spaces should
    be quoted.
    Use \code{R CMD INSTALL --help} for the current list of options.}
}
\description{
  To install packages into the default library tree (which is rooted at
  \file{\$R\_HOME/library}), do \code{R CMD INSTALL pkgs}.

  To install into the library tree \code{lib} instead of the default
  one, use \code{R CMD INSTALL -l lib pkgs}.
}
\details{
  Windows Perl and the files for installing from source packages need to
  be installed.
  
  Both \code{lib} and the elements of \code{pkgs} may be absolute or
  relative path names of directories.  \code{pkgs} may also contain
  names of package archive files of the form \file{pkg\_version.tar.gz}
  as obtained from CRAN (these are then extracted in a temporary
  directory \file{R.INSTALL} of the current directory).

  Which forms of help are produced is controlled by the \option{--docs}
  option which takes values \code{none} or \code{normal} (text and HTML)
  or \code{chm} (the default, text, HTML and CHTML).

  If the attempt to install the package fails, leftovers are removed.
  If the package was already installed, the old version is restored.
  This happens either if a command encounters an error or if the
  install is interrupted from the keyboard.

  Use \code{R CMD INSTALL --help} for more usage information.
}

\section{Packages using the methods package}{
  Packages that require the methods package, and that use functions such
  as \code{\link{setMethod}} or \code{\link{setClass}},
  should be installed either (preferably) using lazy-loading or by
  creating a binary image: use the flags \code{LazyLoad} and
  \code{SaveImage} in the \file{DESCRIPTION} file to ensure this.
}
\seealso{
  \code{\link{REMOVE}} and \code{\link{library}} for information on
  using several library trees;
  \code{\link{update.packages}} for automatic update of packages using
  the internet;
  the section on \dQuote{Add-on packages} in \dQuote{R Installation and
    Administration} and
  the chapter on \dQuote{Creating R packages} in \dQuote{Writing \R
    Extensions}
  (see the \file{doc/manual} subdirectory of the \R source tree).
}
\keyword{utilities}
