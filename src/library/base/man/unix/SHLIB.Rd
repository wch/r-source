\name{SHLIB}
\title{Build Shared Library for Dynamic Loading}
\alias{SHLIB}
\usage{R SHLIB [-o libname] files}
\arguments{
  \item{files}{a list of names of (typically) source files to be
    compiled and included in the library.  You can also include the name
    of object files which are automagically made from their sources.}
  \item{libname}{the full name of the shared library to be built,
    including the extension (typically \file{.so} on Unix systems).  If
    not given, the name of the library is taken from the first source
    file.}
}
\description{
  The given source files are first compiled using \code{R COMPILE}.  All
  specified object files are then linked into a shared library which can
  be loaded into R using \code{dyn.load()}.  
}
\seealso{
  \code{\link{COMPILE}}, \code{\link{dyn.load}}
}
\keyword{utilities}
