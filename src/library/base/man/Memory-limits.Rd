\name{Memory-limits}
\alias{Memory-limits}
\concept{memory}
\concept{limits}
\title{Memory Limits in R}
\description{
  \R holds objects it is using in memory.  This help file documents the
  current design limitations on large objects: these differ between
  32-bit and 64-bit builds of \R.
}
\details{
  \R holds all objects in memory, and there are limits based on the
  amount of memory that can be used by all objects:
  \itemize{
    \item There may be limits on the size of the heap and the number of
    cons cells allowed -- see \code{\link{Memory}} -- but these are
    usually not imposed.
#ifdef unix
    \item There is a limit on the address space of a single process such
    as the \R executable.  This is system-specific, but for 32-bit OSes
    imposes a limit of no more than 4Gb.  It is often rather less.
    \item The environment may impose limitations on the resources
    available to a single process -- see the OS/shell's help on commands such
    as \code{limit} or \code{ulimit}.
#endif
#ifdef windows
    \item There is a limit on the address space of a single process such
    as the \R executable.  This is 2Gb under Windows unless both the OS
    and the \R executable have been changed to allow more (up to 3Gb?).
    See the \file{rw-FAQ} for how to do so.
    \item Under Windows, \R does impose limits on the total memory
    allocation available to a single run, as the OS provides no way to
    do so.  See \code{\link{memory.size}} and \code{\link{memory.limit}}.
#endif
  }
  Error messages beginning \code{cannot allocate vector of size}
  indicate a failure to obtain memory, either because the size exceeded
  the address-space limit for a process or, more likely, because the
  system was unable to provide the memory.

  There are also limits on individual objects.  On all versions of \R,
  the maximum length (number of elements) of a vector is
  \eqn{2^{31} - 1 \approx 2\thinspace 10^9}{2^31 - 1 ~ 2*10^9}, as
  lengths are stored as signed integers.  In addition, the storage space
  cannot exceed the address limit, and if you try to exceed that limit,
  the error message begins \code{cannot allocate vector of length}.
  The number of characters in a character string is in theory only
  limited by the address space.
}
\seealso{
  \code{\link{object.size}(a)} for the (approximate) size of \R object
  \code{a}.
}
\keyword{environment}
