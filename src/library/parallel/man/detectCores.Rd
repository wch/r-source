% File src/library/parallel/man/detectCores.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2011-2020 R Core Team
% Distributed under GPL 2 or later

\name{detectCores}
\alias{detectCores}
\title{Detect the Number of CPU Cores}
\description{
  Attempt to detect the number of CPU cores on the current host.
}
\usage{
detectCores(all.tests = FALSE, logical = TRUE)
}
\arguments{
  \item{all.tests}{Logical: if true apply all known tests.}
  \item{logical}{Logical: if possible, use the number of physical CPUs/cores
    (if \code{FALSE}) or logical CPUs (if \code{TRUE}).  Currently this
    is honoured only on macOS, Solaris and Windows.}
}
\details{
  This attempts to detect the number of available CPU cores.

  It has methods to do so for Linux, macOS, FreeBSD, \I{OpenBSD}, Solaris
  and Windows.  \code{detectCores(TRUE)} could be tried on other
  Unix-alike systems.
}
\value{
  An integer, \code{NA} if the answer is unknown.

  Exactly what this represents is OS-dependent: where possible by
  default it counts logical (e.g., hyperthreaded) CPUs and not physical
  cores or packages.

  Under macOS there is a further distinction between \sQuote{available in
    the current power management mode} and \sQuote{could be available
    this boot}, and this function returns the first.

  On \I{Sparc} Solaris \code{logical = FALSE} returns the number of physical
  cores and \code{logical = TRUE} returns the number of available
  hardware threads. (Some \I{Sparc} CPUs have multiple cores per CPU, others
  have multiple threads per core and some have both.)  For example, the
  \I{UltraSparc} T2 CPU in the former CRAN check server was a single
  physical CPU with 8 cores, and each core supports 8 hardware threads.
  So \code{detectCores(logical = FALSE)} returns 8, and
  \code{detectCores(logical = TRUE)} returns 64.

  Where virtual machines are in use, one would hope that the result
  for \code{logical = TRUE} represents the number of CPUs available (or
  potentially available) to that particular \abbr{VM}.
}
\author{
  Simon Urbanek and Brian Ripley
}
\note{
  This is not suitable for use directly for the \code{mc.cores} argument
  of \code{mclapply} nor specifying the number of cores in
  \code{makeCluster}.  First because it may return \code{NA}, second
  because it does not give the number of \emph{allowed} cores, and third
  because on \I{Sparc} Solaris and some Windows boxes it is not reasonable
  to try to use all the logical CPUs at once.
}
\examples{
detectCores()
detectCores(logical = FALSE)
}
