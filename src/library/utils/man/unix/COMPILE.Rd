% File src/library/utils/man/unix/COMPILE.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{COMPILE}
\alias{COMPILE}
\title{Compile Files for Use with R}
\description{
  Compile given source files so that they can subsequently be collected
  into a shared object using \command{R CMD SHLIB} or an executable
  program using \command{R CMD LINK}.
}
\usage{
\special{R CMD COMPILE [options] srcfiles}
}
\arguments{
  \item{srcfiles}{A list of the names of source files to be compiled.
    Currently, C, C++, Objective C, Objective C++ and Fortran are
    supported; the corresponding files should have the extensions
    \file{.c}, \file{.cc} (or \file{.cpp}), \file{.m},
    \file{.mm} (or \file{.M}), \file{.f} and \file{.f90} or \file{.f95},
    respectively.}
  \item{options}{A list of compile-relevant settings, or for obtaining
    information about usage and version of the utility.}
}
\details{
  \command{R CMD SHLIB} can both compile and link files into a
  shared object: since it knows what run-time libraries are needed
  when passed C++, Fortran and Objective C(++) sources, passing source
  files to \command{R CMD SHLIB} is more reliable.
  
  Ratfor is not supported.  If you have Ratfor source code, you need to
  convert it to FORTRAN.  (On some Solaris systems mixing Ratfor and
  FORTRAN code will work.)

  Objective C and Objective C++ support is optional and will work only
  if the corresponding compilers were available at \R configure time:
  their main usage is on OS X.

  Compilation arranges to include the paths to the \R public C/C++ headers.

  As this compiles code suitable for incorporation into a shared object,
  it generates PIC code: that might occasionally be undesirable for the
  main code of an executable program.

  This is a \command{make}-based facility, so will not compile a source file
  if a newer corresponding \file{.o} file is present.
}
\note{
  Some binary distributions of \R have \code{COMPILE} in a separate
  bundle, e.g. an \code{R-devel} RPM.
}
\seealso{
  \code{\link{LINK}}, \code{\link{SHLIB}}, \code{\link{dyn.load}};
  the section on \dQuote{Customizing compilation under Unix} in
  \dQuote{R Administration and Installation}
  (see the \file{doc/manual} subdirectory of the \R source tree).
}
\keyword{utilities}
