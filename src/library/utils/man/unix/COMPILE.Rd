\name{COMPILE}
\alias{COMPILE}
\title{Compile Files for Use with R}
\description{
  Compile given source files so that they can subsequently be collected
  into a shared library using \code{R CMD SHLIB} and be loaded into R
  using \code{dyn.load()}.
}
\usage{R CMD COMPILE [options] srcfiles}
\arguments{
  \item{srcfiles}{A list of the names of source files to be compiled.
    Currently, C, C++ and FORTRAN are supported; the corresponding files
    should have the extensions \file{.c}, \file{.cc} (or \file{.cpp} or
    \file{.C}), and \file{.f}, respectively.}
  \item{options}{A list of compile-relevant settings, such as special
    values for \code{CFLAGS} or \code{FFLAGS}, or for obtaining
    information about usage and version of the utility.}
}
\details{
  Note that Ratfor is not supported.  If you have Ratfor source code,
  you need to convert it to FORTRAN.  On many Solaris systems mixing
  Ratfor and FORTRAN code will work.
}
\note{
  Some binary distributions of \R have \code{COMPILE} in a separate
  bundle, e.g. an \code{R-devel} RPM.
}
\seealso{
  \code{\link{SHLIB}},
  \code{\link{dyn.load}};
  the section on \dQuote{Customizing compilation under Unix} in
  \dQuote{R Administration and Installation}
  (see the \file{doc/manual} subdirectory of the \R source tree).  
}
\keyword{utilities}
