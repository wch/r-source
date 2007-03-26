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
\special{R CMD build [options] pkgdirs}
\special{R CMD check [options] pkgdirs}
}
\arguments{
  \item{pkgdirs}{a list of names of directories with sources of \R
    add-on packages.}
  \item{options}{further options to control the processing, or for
    obtaining information about usage and version of the utility.}
}
\details{
  \code{R CMD check}
  checks \R add-on packages from their sources, performing a wide
  variety of diagnostic checks.

  \code{R CMD build}
  builds \R source or binary packages from their sources.  The name(s)
  of the packages are taken from the \file{DESCRIPTION} files and not
  from the directory names (as from \R 2.5.0).

  Use
  \code{R CMD foo --help}
  to obtain usage information on utility \code{foo}.

  Several of the options to \code{build --binary} are passed to
  \code{\link{INSTALL}} so consult its help for the details.
}
#ifdef windows
\note{
  These may not work correctly under Windows 95/98/ME because of
  problems Perl has launching programs on those limited OSes.

  They make use of a temporary directory specified by the environment
  variable \code{TMPDIR} and defaulting to \code{c:/TEMP}.  Do ensure
  that if set forward slashes are used.
}
#endif
\seealso{
  The sections on \dQuote{Checking and building packages} and
  \dQuote{Processing Rd format} in \dQuote{Writing \R Extensions}
#ifdef unix
  (see the \file{doc/manual} subdirectory of the \R source tree).
#endif
#ifdef windows
  (see the Manuals sub-menu of the Help menu on the console).
#endif

  \code{\link{INSTALL}} is called by \code{build --binary}.
}
\keyword{utilities}
