% File src/library/utils/man/windows/memory.size.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{memory.size}
\alias{memory.size}
\alias{memory.limit}
\title{Report on Memory Allocation}
\description{
  \code{memory.size} reports the current or maximum memory allocation of
  the \code{malloc} function used in this version of \R.

  \code{memory.limit} reports or increases the limit in force
  on the total allocation.
}
\usage{
memory.size(max = FALSE)

memory.limit(size = NA)
}
\arguments{
  \item{max}{logical.  If \code{TRUE} the maximum amount of memory
    obtained from the OS is reported, if \code{FALSE} the amount
    currently in use, if \code{NA} the memory limit.}
  \item{size}{numeric.  If \code{NA} report the memory limit, otherwise
    request a new limit, in Mb.  Only values of up to 4095 are allowed on
    32-bit \R builds, but see \sQuote{Details}.}
}
\details{
  Command-line flag \option{--max-mem-size} sets the maximum value of
  obtainable memory (including a very small amount of housekeeping
  overhead).  This cannot exceed 3Gb on 32-bit Windows, and most
  versions are limited to 2Gb.  The minimum is currently 32Mb.

  If 32-bit \R is run on most 64-bit versions of Windows the maximum
  value of obtainable memory is just under 4Gb.  For a 64-bit versions
  of \R under 64-bit Windows the limit is currently 8Tb.

  Memory limits can only be increased.

  Environment variable \env{R_MAX_MEM_SIZE} provides another way to
  specify the initial limit.
}
\value{
  Size in Mb (1048576 bytes), rounded to 0.01 Mb for \code{memory.size}
  and rounded down for \code{memory.limit}.
}
\note{
  These functions exist on other platforms but always report infinity as
  \R does itself provide limits on memory allocation---the OS's own
  facilities can be used. 
}
\seealso{
  \link{Memory-limits} for other limits.

  The rw-FAQ for more details and references.
}
\examples{
memory.size()
memory.size(TRUE)
memory.limit()
}
\keyword{utilities}
