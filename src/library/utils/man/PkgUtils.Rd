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
#ifdef unix
  \code{R CMD check}
#endif
#ifdef windows
  \code{Rcmd check}
#endif
  checks \R add-on packages from their sources, performing a wide
  variety of diagnostic checks.

#ifdef unix
  \code{R CMD build}
#endif
#ifdef windows
  \code{Rcmd build}
#endif
  builds \R source or binary packages from their sources.  It will
  create index files in the sources if necessary, so it is often helpful
  to run \code{build} before \code{check}.

  Use
#ifdef unix
  \code{R CMD foo --help}
#endif
#ifdef windows
  \code{Rcmd foo --help}
#endif
  to obtain usage information on utility \code{foo}.

  Several of the options to \code{build --binary} are passed to
  \code{\link{INSTALL}} so consult its help for the details.
}
#ifdef windows
\note{
  These may not work correctly under Windows 95/98/ME because of
  problems Perl has launching programs on those limited OSes.

  They make use of a temporary directory specified by the enironment
  variable \code{TMPDIR} and defaulting to \code{c:/TEMP}.  Do ensure
  that if set forward slashes are used.
}
#endif
\seealso{
  The chapter \dQuote{Processing Rd format} in \dQuote{Writing \R
    Extensions}
#ifdef unix
  (see the \file{doc/manual} subdirectory of the \R source tree).
#endif
#ifdef windows
  (see the Manuals sub-menu of the Help menu on the console).
#endif

  \code{\link{INSTALL}} is called by \code{build --binary}.
}
\keyword{utilities}
