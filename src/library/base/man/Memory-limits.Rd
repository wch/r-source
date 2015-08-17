% File src/library/base/man/Memory-limits.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{Memory-limits}
\alias{Memory-limits}
\concept{memory}
\concept{limits}
\title{Memory Limits in R}
\description{
  \R holds objects it is using in virtual memory.  This help file
  documents the current design limitations on large objects: these
  differ between 32-bit and 64-bit builds of \R.
}
\details{
  Currently \R runs on 32- and 64-bit operating systems, and most 64-bit
  OSes (including Linux, Solaris, Windows and OS X) can run either
  32- or 64-bit builds of \R.  The memory limits depends mainly on the
  build, but for a 32-bit build of \R on Windows they also depend on the
  underlying OS version.

  \R holds all objects in virtual memory, and there are limits based on the
  amount of memory that can be used by all objects:
  \itemize{
    \item There may be limits on the size of the heap and the number of
    cons cells allowed -- see \code{\link{Memory}} -- but these are
    usually not imposed.
    \item There is a limit on the (user) address space of a single
    process such as the \R executable.  This is system-specific, and can
    depend on the executable.
    \item The environment may impose limitations on the resources
    available to a single process: Windows' versions of \R do so directly.
  }
  Error messages beginning \code{cannot allocate vector of size}
  indicate a failure to obtain memory, either because the size exceeded
  the address-space limit for a process or, more likely, because the
  system was unable to provide the memory.  Note that on a 32-bit build
  there may well be enough free memory available, but not a large enough
  contiguous block of address space into which to map it.

  There are also limits on individual objects. The storage space
  cannot exceed the address limit, and if you try to exceed that limit,
  the error message begins \code{cannot allocate vector of length}.
  The number of bytes in a character string is limited to
  \eqn{2^{31} - 1 \approx 2\thinspace 10^9}{2^31 - 1 ~ 2*10^9},
  which is also the limit on each dimension of an array.
}
\section{Unix}{
  The address-space limit is system-specific: 32-bit OSes
  imposes a limit of no more than 4Gb: it is often 3Gb.  Running
  32-bit executables on a 64-bit OS will have similar limits: 64-bit
  executables will have an essentially infinite system-specific limit
  (e.g., 128Tb for Linux on x86_64 cpus).

  See the OS/shell's help on commands such as \code{limit} or
  \code{ulimit} for how to impose limitations on the resources available
  to a single process.  For example a \command{bash} user could use
\preformatted{ulimit -t 600 -v 4000000
}
  whereas a \command{csh} user might use
\preformatted{limit cputime 10m
limit vmemoryuse 4096m
}
  to limit a process to 10 minutes of CPU time and (around) 4Gb of
  virtual memory.  (There are other options to set the RAM in use, but they
  are not generally honoured.)
}
\section{Windows}{
  The address-space limit is 2Gb under 32-bit Windows unless the OS's
  default has been changed to allow more (up to 3Gb).  See
  \url{https://www.microsoft.com/whdc/system/platform/server/PAE/PAEmem.mspx}
  and
  \url{https://msdn.microsoft.com/en-us/library/bb613473(VS.85).aspx}.
  Under most 64-bit versions of Windows the limit for a 32-bit build
  of \R is 4Gb: for the oldest ones it is 2Gb.  The limit for a 64-bit
  build of \R (imposed by the OS) is 8Tb.

  It is not normally possible to allocate as much as 2Gb to a single
  vector in a 32-bit build of \R even on 64-bit Windows because of
  preallocations by Windows in the middle of the address space.

  Under Windows, \R imposes limits on the total memory allocation
  available to a single session as the OS provides no way to do so: see
  \code{\link{memory.size}} and \code{\link{memory.limit}}.
}
\seealso{
  \code{\link{object.size}(a)} for the (approximate) size of \R object
  \code{a}.
}
\keyword{environment}
