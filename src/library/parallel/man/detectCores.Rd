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
    (if \code{FALSE}) or logical CPUs (if \code{TRUE}).  The default is
    \code{TRUE} on Windows and \code{FALSE} elsewhere.}
}
\details{
  This attempts to detect the number of CPU cores in the current
  machine.

  It has methods to do so for Linux, OS X, FreeBSD, OpenBSD, Solaris,
  Irix and Windows.  \code{detectCores(TRUE)} could be tried on other
  Unix-alike systems.
%% see also http://thedaneshproject.com/posts/how-to-find-the-number-of-physical-cpus-in-linux/

  Currently \code{logical} is supported on Solaris and Windows only.
}
\value{
  An integer, \code{NA} if the answer is unknown.

  Exactly what this represents is OS-dependent: where possible by
  default it counts physical cores and not logical (e.g., hyperthreaded)
  CPUs.  However, on Linux, OS X, FreeBSD and OpenBSD it currently
  returns the number of logical CPUs.

%%   Not yet: seems to need > 10.9.  
%%   On OS X there is a further distinction between \sQuote{available in
%%   the current power management mode} and \sQuote{that could be available
%%   this boot}, and this function returns the first.

  On Windows the default is the number of logical CPUs.
#ifdef windows
  Only versions of Windows since XP SP3 are supported.  Microsoft
  documents that \code{logical = FALSE} it will report the number of
  cores on Vista or later, but the number of physical CPU packages on XP
  or Server 2003: however it reported correctly on the XP systems we
  tested.
#endif
% https://msdn.microsoft.com/en-us/library/ms683194%28v=VS.85%29.aspx

  Currently \code{logical} makes a difference on Sparc Solaris: there
  \code{logical = FALSE} returns the number of physical cores and
  \code{logical = TRUE} returns the number of available hardware
  threads. (Some Sparc CPUs which do have multiple cores per CPU, others
  have multiple threads per core and some have both.)  For example, the
  UltraSparc T2 CPU in the CRAN check server is a single physical CPU
  with 8 cores, and each core supports 8 hardware threads.  So
  \code{detectCores(logical = FALSE)} returns 8, and
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
  \code{makeCluster}.  First because it may return \code{NA}, and second
  because it does not give the number of \emph{allowed} cores.
}
\examples{
detectCores()
#ifdef unix
detectCores(logical = TRUE)
#endif
#ifdef windows
detectCores(logical = FALSE)
#endif
}
