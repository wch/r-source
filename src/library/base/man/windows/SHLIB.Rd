\name{SHLIB}
\title{Build a DLL for Dynamic Loading}
\alias{SHLIB}
\usage{Rcmd SHLIB [-o dllname] files}
\arguments{
  \item{files}{a list of names of (typically) source files to be
    compiled and included in the library.  You can also include the name
    of object files which are automagically made from their sources.}
  \item{dllname}{the full name of the shared library to be built,
    including the extension \file{.dll}.  If
    not given, the name of the library is taken from the first source
    file.}
}
\description{
  The given source files are first compiled.  All
  specified object files are then linked into a shared library which can
  be loaded into \R using \code{dyn.load()}.  
}
\seealso{
  \code{\link{dyn.load}}
}
\keyword{utilities}
