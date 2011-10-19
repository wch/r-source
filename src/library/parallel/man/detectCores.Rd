% File src/library/parallel/man/detectCores.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2011 R Core Development Team
% Distributed under GPL 2 or later

\name{detectCores}
\alias{detectCores}
\title{Detect the Number of CPU Cores}
\description{
  Attempt to detect the number of CPU cores on the current host.
}
\usage{
#ifdef windows
detectCores(all.tests = FALSE, logical = TRUE)
#endif
#ifdef unix
detectCores(all.tests = FALSE, logical = FALSE)
#endif
}
\arguments{
  \item{all.tests}{Logical: if true apply all known tests.}
  \item{logical}{Logical: if possible, use the number of physical CPUs/cores
    (if \code{FALSE}) or logical CPUs (if \code{TRUE}.}
}
\details{
  This attempts to detect the number of CPU cores in the current
  machine.

  It has methods to do so for Linux, Darwin (Mac OS X), FreeBSD, Solaris,
  Irix and Windows.  \code{detectCores(TRUE)} could be tried on other
  Unix-alike systems.
}
\value{
  An integer, \code{NA} if the answer is unknown.
  
  Exactly what this represents is OS-dependent: where possible by
  default it represents physical cores and not logical
  (e.g. hyperthreaded) CPUs.  Unfortunately on Windows only the number
  of logical CPUs seems to be available on non-server versions of the
  OS.

  Currently \code{logical} only makes a difference on Solaris: there
  \code{logical = FALSE} returns the number of physical CPUs (not cores)
  and \code{logical = TRUE} returns the number of logical CPUs, which is
  the number of cores on AMD/Intel hardware, but the number of available
  hardware threads on Sparc hardware (some versions of which do have
  multiple cores per CPU, and others have multiple threads per core).
  For example, the UltraSparc T2 CPU in the CRAN check server is a
  single physical CPU with 8 cores, and each core supports 8 hardware
  threads.  So \code{detectCores(logical=FALSE)} returns 1, and
  \code{detectCores(logical=TRUE)} returns 64, neither of which is very
  useful!

  Where virtual machines are in use, one would hope that the result
  represents the number of CPUs available (or potentially available) to
  that particular VM.
}
\author{
  Simon Urbanek and Brian Ripley
}
\examples{
detectCores()
}
