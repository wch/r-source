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
R CMD build [options] pkgdirs
R CMD check [options] pkgdirs
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

  Use \code{R CMD foo --help} to obtained usage information on utility
  \code{foo}.
}
\seealso{
  chapter ``Checking and building packages'' in ``Writing \R
  Extensions'' (see the \file{doc/manual} subdirectory of the \R source
  tree).
}
\keyword{utilities}
