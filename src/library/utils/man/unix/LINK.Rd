% File src/library/utils/man/unix/LINK.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{LINK}
\alias{LINK}
\title{Create Executable Programs}
\description{
  Front-end for creating executable programs.
}
\usage{
\special{R CMD LINK [options] linkcmd}
}
\arguments{
  \item{linkcmd}{a list of commands to link together suitable object
    files (include library objects) to create the executable program.}
  \item{options}{further options to control the linking, or for
    obtaining information about usage and version.}
}
\details{
  The linker front-end is useful in particular when linking against the
  \R shared or static library: see the examples.

  The actual linking command is constructed by the version of
  \command{libtool} installed at \file{\env{R_HOME}/bin}.

  \command{R CMD LINK --help} gives usage information.
}
\note{
  Some binary distributions of \R have \code{LINK} in a separate
  bundle, e.g. an \code{R-devel} RPM.
}
\seealso{
  \code{\link{COMPILE}}.
}
\examples{\dontrun{## examples of front-ends linked against R.
## First a C program
CC=`R CMD config CC`
R CMD LINK $CC -o foo foo.o `R CMD config --ldflags`

## if Fortran code has been compiled into ForFoo.o
FLIBS=`R CMD config FLIBS`
R CMD LINK $CC -o foo foo.o ForFoo.o `R CMD config --ldflags` $FLIBS

## And for a C++ front-end
CXX=`R CMD config CXX`
R CMD COMPILE foo.cc
R CMD LINK $CXX -o foo foo.o `R CMD config --ldflags`
}}
\keyword{utilities}
