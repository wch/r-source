\name{SHLIB}
\alias{SHLIB}
\title{Build Shared Library for Dynamic Loading}
\description{
  Compile the given source files and then link all specified object
  files into a shared library which can be loaded into R using
  \code{dyn.load} or \code{library.dynam}.
}
\usage{
\special{R CMD SHLIB [options] [-o libname] files}
}
\arguments{
  \item{files}{a list specifying the object files to be included in the
    shared library.  You can also include the name of source files, for
    which the object files are automagically made from their sources.}
  \item{libname}{the full name of the shared library to be built,
    including the extension (typically \file{.so} on Unix systems).  If
    not given, the name of the library is taken from the first file.}
  \item{options}{Further options to control the processing.  Use
    \code{R CMD SHLIB --help} for a current list.}
}
\details{
  \code{R CMD SHLIB} is the mechanism used by \code{\link{INSTALL}} to
  compile source code in packages. Please consult section
  \sQuote{Creating shared objects} in the manual
  \sQuote{Writing R Extensions} for how to customize it (for example to
  add \code{cpp} flags and to add libraries to the link step) and for
  details of some of its quirks.
}
\note{
  Some binary distributions of \R have \code{SHLIB} in a separate
  bundle, e.g. an \code{R-devel} RPM.
}
\seealso{
  \code{\link{COMPILE}},
  \code{\link{dyn.load}},
  \code{\link{library.dynam}}.

  The section on \dQuote{Customizing compilation under Unix} in
  \dQuote{R Administration and Installation}
  (see the \file{doc/manual} subdirectory of the \R source tree).  

  The \sQuote{Writing R Extensions} manual.
}
\keyword{utilities}
