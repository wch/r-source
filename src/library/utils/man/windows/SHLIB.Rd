\name{SHLIB}
\alias{SHLIB}
\title{Build a DLL for Dynamic Loading}
\description{
  The given source files are first compiled.  All specified object files
  are then linked into a shared library which can be loaded into \R
  using \code{dyn.load} or \code{library.dynam}.
}
\usage{
\special{R CMD SHLIB [options] [-o dllname] files}
}
\arguments{
  \item{files}{a list of names of (typically) source files to be
    compiled and included in the library.  You can also include the names
    of object files which are automagically made from their sources,
    and archive (\code{*.a}) and library linking commands.
  }
  \item{dllname}{the name of the shared library to be built, optionally
    including the extension \file{.dll}.  If not given, the name of the
    DLL is taken from the first source or object file specified.}
  \item{options}{Further options to control the processing.  Use
    \code{R CMD SHLIB --help} for a current list.  The most useful one
    is \code{-d} to build a debug DLL.}
}
\details{
  \code{R CMD SHLIB} uses the mechanism as used by \code{\link{INSTALL}}
  to compile source code in packages.  To use \code{SHLIB} you need to
  have installed (from the \R installer) the files for compiling source
  packages as well as the tools described in the \sQuote{R Installation and 
  Administration} manual.

  Please consult section \sQuote{Creating shared objects} in the manual
  \sQuote{Writing R Extensions} for how to customize it (for example to
  add \code{cpp} flags and to add libraries to the link step) and for
  details of some of its quirks.

  Items in \code{files} with extensions \code{.c}, \code{.cpp},
  \code{.cc}, \code{.C}, \code{.f}, \code{.f90}, \code{.f95} and \code{.m},
  are regarded as source files, and those with extension \code{.o} as
  object files.  All other items are passed to the linker.

  If there is an exports file \file{dllname-win.def} in the current
  directory it will be used, otherwise all entry points in objects (but
  not libraries) will be exported from the DLL.
}
\seealso{
  \code{\link{dyn.load}}, \code{\link{library.dynam}}.

  The \sQuote{R Installation and Administration} and \sQuote{Writing R 
  Extensions} manuals.
}
\examples{\dontrun{
rcmd SHLIB -o my.dll a.f b.f -L/AMD/acml3.5.0/gnu32/lib -lacml
}}
\keyword{utilities}
