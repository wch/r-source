\name{memory.size}
\alias{memory.size}
\alias{memory.limit}
\title{Report on Memory Allocation}
\description{
  \code{memory.size} and \code{memory.limit} are used to manage the
  total memory allocation on Windows.  On other platforms these are
  stubs which report infinity with a warning.
}
\usage{
memory.size(max = FALSE)

memory.limit(size = NA)
}
\arguments{
  \item{max}{logical. If true the maximum amount of memory obtained from
    the OS is reported, otherwise the amount currently in use.}
  \item{size}{numeric. If \code{NA} report the memory size, otherwise
    request a new limit, in Mb.}
}
\details{
  To restrict memory usage on a Unix-alike use the facilities of the
  shell used to launch \R, e.g. \code{limit} or \code{ulimit}.
}
\value{
  Size in bytes: always \code{Inf}.
}
\seealso{
  \link{Memory-limits} for other limits.
}
\keyword{utilities}
