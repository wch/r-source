\name{PkgUtils}
\alias{build}
\alias{check}
\title{Utilities for Building and Checking Add-on Packages}
\description{
  Utilities for checking whether the sources of an \R add-on package
  work correctly, and for building a source or binary package from
  them.
}
\usage{
#ifdef unix
R CMD build [options] pkgdirs
R CMD check [options] pkgdirs
#endif
#ifdef windows
Rcmd build [options] pkgdirs
Rcmd check [options] pkgdirs
#endif
}
\arguments{
  \item{pkgdirs}{a list of names of directories with sources of \R
    add-on packages.}
  \item{options}{further options to control the processing, or for
    obtaining information about usage and version of the utility.}
}
\details{
  \code{R CMD check} checks \R add-on packages from their sources,
  performing a wide variety of diagnostic checks.

  \code{R CMD build} builds \R source or binary packages from their
  sources.

#ifdef unix
  Use \code{R CMD foo --help} to obtained usage information on utility
  \code{foo}.
#endif
#ifdef windows
  Use \code{Rcmd foo --help} to obtained usage information on utility
  \code{foo}.
#endif
}
\seealso{
  The chapter ``Processing Rd format'' in ``Writing \R Extensions''
#ifdef unix
  (see the \file{doc/manual} subdirectory of the \R source tree).
#endif
#ifdef windows
  (see the Manuals sub-menu of the Help menu on the console).
#endif
}
\keyword{utilities}
