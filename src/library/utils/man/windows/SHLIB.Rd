\name{SHLIB}
\title{Build a DLL for Dynamic Loading}
\alias{SHLIB}
\usage{R CMD SHLIB [-o dllname] files}
\arguments{
  \item{files}{a list of names of (typically) source files to be
    compiled and included in the library.  You can also include the names
    of object files which are automagically made from their sources.}
  \item{dllname}{the full name of the shared library to be built,
    including the extension \file{.dll}.  If not given, the name of
    the DLL is taken from the first source file.}
}
\description{
  The given source files are first compiled.  All
  specified object files are then linked into a shared library which can
  be loaded into \R using \code{dyn.load} or \code{library.dynam}.  
}
\details{
  To use \code{SHILB} you need to have installed (from the \R installer)
  the files for compiling source packages and the tools described in the
  file \file{readme.packages} in the top directory of the \R installation.
}
\seealso{
  \code{\link{dyn.load}}, \code{\link{library.dynam}}
}
\keyword{utilities}
