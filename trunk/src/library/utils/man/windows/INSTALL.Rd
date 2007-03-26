\name{INSTALL}
\alias{INSTALL}
\title{Install Add-on Packages from Sources}
\description{
  Utility for installing add-on source packages.
}
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
\details{
  If used as \code{R CMD INSTALL pkgs} without explicitly specifying
  \code{lib}, packages are installed into the library tree rooted at the
  first directory in the library path which would be used by \R run in
  the current environment.

  To install into the library tree \code{lib}, use
  \code{R CMD INSTALL -l lib pkgs}.  This prepends \code{lib} to
  \env{R\_LIBS} for duration of the install, so required packages in the
  installation directory will be found (and used in preference to those
  in other libraries).

  Windows Perl and the files for installing from source packages need to
  be installed.
  
  Both \code{lib} and the elements of \code{pkgs} may be absolute or
  relative path names of directories.  \code{pkgs} may also contain
  names of package archive files of the form \file{pkg\_version.tar.gz}
  as obtained from CRAN---these are then extracted to a temporary
  directory \file{R.INSTALL.????} of the current directory.

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
  the internet (or other \R level installation of packages, such as by
  \code{install.packages}).

  The section on \dQuote{Add-on packages} in \dQuote{R Installation and
    Administration} and the chapter on \dQuote{Creating R packages} in
  \dQuote{Writing \R Extensions} (see the Help menu in \code{RGui},
  \code{\link{RShowDoc}} and the \file{doc/manual} subdirectory of the
  \R source tree).
}
\keyword{utilities}
