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
detectCores(all.tests = FALSE)
}
\arguments{
  \item{all.tests}{Logical: if true apply all known tests.}
}
\details{
  This attempts to detect the number of CPU cores on the current
  machine.

  It has methods to do so for Linux, Darwin (Mac OS X), FreeBSD, Solaris
  and Irix.  \code{detectCores(TRUE)} could be tried on other Unix-alike
  systems.
}
\value{
  An integer.  Exactly what this represents is OS-dependent: where
  possible it represents physical cores and not virtual
  (e.g. hyperthreaded) CPUs.
}
\examples{
detectCores()
}
