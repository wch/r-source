\name{SHLIB}
\title{Build Shared Library for Dynamic Loading}
\alias{SHLIB}
\usage{R SHLIB libname objfiles}
\arguments{
  \item{libname}{The full name of the shared library to be built,
    including the extension (typically \file{.so} on Unix systems).}
  \item{objfiles}{A list of the names of object files, typically
    obtained via \code{R COMPILE}, to be included in the library.}
}
\description{
  The given object files are linked into a shared library which can then
  be loaded into R using \code{dyn.load()}.
}
\seealso{
  \code{\link{COMPILE}}, \code{\link{dyn.load}}
}
