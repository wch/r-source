\name{COMPILE}
\title{Compile Files for Use with R}
\alias{COMPILE}
\usage{R COMPILE [options] srcfiles}
\arguments{
  \item{srcfiles}{A list of the names of source files to be compiled.
    Currently, only C and FORTRAN are supported; the corresponding files
    should have the extensions \file{.c} and \file{.f}, respectively.}
  \item{options}{A list of compile-relevant settings, such as special
    values for \code{CFLAGS} or \code{FFLAGS}.}
}
\description{
  The given source files are compiled so that they can subsequently be
  collected into a shared library using \code{R SHLIB} and be loaded
  into R using \code{dyn.load()}.
}
\seealso{
  \code{\link{SHLIB}}, \code{\link{dyn.load}}
}
\keyword{utilities}
