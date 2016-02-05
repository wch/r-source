% File src/library/parallel/man/detectCores.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2011-2016 R Core Team
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
    is honoured only on Linux, OS X, Sparc Solaris and Windows.}
}
\details{
  This attempts to detect the number of available CPU cores.

  It has methods to do so for Linux, OS X, FreeBSD, OpenBSD, Solaris,
  Irix and Windows.  \code{detectCores(TRUE)} could be tried on other
  Unix-alike systems.

  \code{logical = FALSE} on OS X may not be supported on all OS versions
  (it uses a command which is  not documented for Mavericks).

  Prior to \R 3.3.0 the default was \code{logical = FALSE} except on
  Windows, but \code{logical = TRUE} had an effect only on Sparc Solaris
  and Windows (where it was the default).
}
\value{
  An integer, \code{NA} if the answer is unknown.

  Exactly what this represents is OS-dependent: where possible by
  default it counts logical (e.g., hyperthreaded) CPUs and not physical
  cores or packages.

  On OS X there is a further distinction between \sQuote{available in
    the current power management mode} and \sQuote{that could be available
    this boot}, and this function returns the first, at least for
  \code{logical = TRUE}.

#ifdef windows
  Only versions of Windows since XP SP3 are supported.  Microsoft
  documents that \code{logical = FALSE} it will report the number of
  cores on Vista or later, but the number of physical CPU packages on XP
  or Server 2003: however it reported correctly on the XP systems we
  tested.
#endif
% https://msdn.microsoft.com/en-us/library/ms683194%28v=VS.85%29.aspx

  On Sparc Solaris \code{logical = FALSE} returns the number of physical
  cores and \code{logical = TRUE} returns the number of available
  hardware threads. (Some Sparc CPUs which do have multiple cores per
  CPU, others have multiple threads per core and some have both.)  For
  example, the UltraSparc T2 CPU in the CRAN check server is a single
  physical CPU with 8 cores, and each core supports 8 hardware threads.
  So \code{detectCores(logical = FALSE)} returns 8, and
  \code{detectCores(logical = TRUE)} returns 64.

  Where virtual machines are in use, one would hope that the result
  represents the number of CPUs available (or potentially available) to
  that particular VM.
}
\author{
  Simon Urbanek and Brian Ripley
}
\note{
  This is not suitable for use directly for the \code{mc.cores} argument
  of \code{mclapply} nor specifying the number of cores in
  \code{makeCluster}.  First because it may return \code{NA}, second
  because it does not give the number of \emph{allowed} cores, and third
  because on Sparc Solaris and some Windows boxes it is not reasonable
  to use all the logical CPUs at once.
}
\examples{
detectCores()
detectCores(logical = FALSE)
}
