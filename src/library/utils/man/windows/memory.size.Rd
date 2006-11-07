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
  \item{max}{logical. If true the maximum amount of memory obtained from
    the OS is reported, otherwise the amount currently in use.}
  \item{size}{numeric. If \code{NA} report the memory size, otherwise
    request a new limit, in Mb.  Values of up to 4096 are allowed, but
    see Details.}
}
\details{
  Command-line flag \code{--max-mem-size} sets the maximum value of
  obtainable memory (including a very small amount of housekeeping
  overhead).  This cannot exceed 3Gb on Windows, and most versions of
  Windows are limited to 2Gb.  The minimum is currently 32Mb.

  Memory limits can only be increased.
}
\value{
  Size in bytes.
}
\seealso{
  \link{Memory-limits} for other limits.

  The rw-FAQ for more details and references.
}
\examples{
memory.size()
memory.size(TRUE)
round(memory.limit()/1048576.0, 2)
}
\keyword{utilities}
