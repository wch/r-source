% File src/library/utils/man/SHLIB.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{SHLIB}
\alias{SHLIB}
\title{Build Shared Object/DLL for Dynamic Loading}
\description{
  Compile the given source files and then link all specified object
  files into a shared object aka DLL which can be loaded into \R using
  \code{dyn.load} or \code{library.dynam}.
}
\usage{
\special{R CMD SHLIB [options] [-o dllname] files}
}
\arguments{
  \item{files}{a list specifying the object files to be included in the
    shared object/DLL.  You can also include the name of source files (for
    which the object files are automagically made from their sources)
    and library linking commands.
  }
  \item{dllname}{the full name of the shared object/DLL to be built,
    including the extension (typically \file{.so} on Unix systems, and
    \file{.dll} on Windows).  If not given, the basename of the object/DLL
    is taken from the basename of the first file.}
  \item{options}{Further options to control the processing.  Use
    \command{R CMD SHLIB --help} for a current list.
#ifdef windows
    The most useful one on Windows is \code{-d} to build a debug DLL.
#endif
  }
}
\details{
  \command{R CMD SHLIB} is the mechanism used by \code{\link{INSTALL}} to
  compile source code in packages.  It will generate suitable
  compilation commands for C, C++, Objective C(++) and Fortran sources: Fortran
  90/95 sources can also be used but it may not be possible to mix these
  with other languages (on most platforms it is possible to mix with C,
  but mixing with C++ rarely works).

  Please consult section \sQuote{Creating shared objects} in the manual
  \sQuote{Writing R Extensions} for how to customize it (for example to
  add \code{cpp} flags and to add libraries to the link step) and for
  details of some of its quirks.

  Items in \code{files} with extensions \file{.c}, \file{.cpp},
  \file{.cc}, \file{.C}, \file{.f}, \file{.f90}, \file{.f95}, \file{.m}
  (ObjC), \file{.M} and \file{.mm} (ObjC++) are regarded as source
  files, and those with extension \file{.o} as object files.  All other
  items are passed to the linker.

  Objective C(++) support is optional when \R was configured: their main
  usage is on OS X.

  Note that the appropriate run-time libraries will be used when linking
  if C++, Fortran or Objective C(++) sources are supplied, but not for
  compiled object files from these languages.

  Option \option{-n} (also known as \option{--dry-run}) will show the
  commands that would be run without actually executing them.
#ifdef windows

  If there is an exports file \file{dllname-win.def} in the current
  directory it will be used, otherwise all entry points in object files
  (but not libraries) will be exported from the DLL.
#endif
}
#ifdef unix
\note{
  Some binary distributions of \R have \code{SHLIB} in a separate
  bundle, e.g., an \code{R-devel} RPM.
}
#endif
\seealso{
#ifdef unix
  \code{\link{COMPILE}},
#endif
  \code{\link{dyn.load}}, \code{\link{library.dynam}}.

  The \sQuote{R Installation and Administration} and \sQuote{Writing R
  Extensions} manuals, including the section on \dQuote{Customizing
  compilation} in the former.
}
#ifdef unix
\examples{\dontrun{
# To link against a library not on the system library paths:
R CMD SHLIB -o mylib.so a.f b.f -L/opt/acml3.5.0/gnu64/lib -lacml
}}
#endif
#ifdef windows
\examples{\dontrun{
# To link against a library not on the system library paths:
Rcmd SHLIB -o my.dll a.f b.f -L/AMD/acml3.5.0/gnu32/lib -lacml
}}
#endif
\keyword{utilities}
