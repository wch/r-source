\name{SHLIB}
\title{Build Shared Library for Dynamic Loading}
\alias{SHLIB}
\description{
  Compile given source files using \code{R CMD COMPILE}, and then link
  all specified object files into a shared library which can be loaded
  into R using \code{dyn.load()}.
}
\usage{R CMD SHLIB [-o libname] files}
\arguments{
  \item{files}{a list specifying the object files to be included in the
    shared library.  You can also include the name of source files, for
    which the object files are automagically made from their sources.}
  \item{libname}{the full name of the shared library to be built,
    including the extension (typically \file{.so} on Unix systems).  If
    not given, the name of the library is taken from the first object
    file.}
}
\seealso{
  \code{\link{COMPILE}}, \code{\link{dyn.load}}
}
\keyword{utilities}
