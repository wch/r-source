\name{COMPILE}
\title{Compile Files for Use with R}
\alias{COMPILE}
\description{
  Compile given source files so that they can subsequently be collected
  into a shared library using \code{R CMD SHLIB} and be loaded into R
  using \code{dyn.load()}.
}
\usage{R CMD COMPILE [options] srcfiles}
\arguments{
  \item{srcfiles}{A list of the names of source files to be compiled.
    Currently, C, C++ and FORTRAN are supported; the corresponding files
    should have the extensions \file{.c}, \file{.cc} (or \file{.cpp} or
    \file{.C}), and \file{.f}, respectively.}
  \item{options}{A list of compile-relevant settings, such as special
    values for \code{CFLAGS} or \code{FFLAGS}, or for obtaining
    information about usage and version of the utility.}
}
\seealso{
  \code{\link{SHLIB}}, \code{\link{dyn.load}}
}
\keyword{utilities}
