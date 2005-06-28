\name{SHLIB}
\title{Build a DLL for Dynamic Loading}
\description{
  The given source files are first compiled.  All specified object files
  are then linked into a shared library which can be loaded into \R
  using \code{dyn.load} or \code{library.dynam}.
}
\alias{SHLIB}
\usage{
\special{R CMD SHLIB [options] [-o dllname] files}
}
\arguments{
  \item{files}{a list of names of (typically) source files to be
    compiled and included in the library.  You can also include the names
    of object files which are automagically made from their sources.}
  \item{dllname}{the full name of the shared library to be built,
    including the extension \file{.dll}.  If not given, the name of
    the DLL is taken from the first source file.}
  \item{options}{Further options to control the processing.  Use
    \code{R CMD SHLIB --help} for a current list.  The most useful one
    is \code{-d} to build a debug DLL.}
}
\details{
  \code{R CMD SHLIB} uses the mechanism as used by \code{\link{INSTALL}}
  to compile source code in packages. To use \code{SHLIB} you need to
  have installed (from the \R installer) the files for compiling source
  packages as well as the tools described in the \sQuote{R Installation and 
  Administration} manual.

  Please consult section \sQuote{Creating shared objects} in the manual
  \sQuote{Writing R Extensions} for how to customize it (for example to
  add \code{cpp} flags and to add libraries to the link step) and for
  details of some of its quirks.
}
\seealso{
  \code{\link{dyn.load}}, \code{\link{library.dynam}}.

  The \sQuote{R Installation and Administration} and \sQuote{Writing R 
  Extensions} manuals.
}
\keyword{utilities}
