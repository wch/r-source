\name{memory.size}
\alias{memory.size}
\alias{memory.limit}
\title{Report on Memory Allocation}
\description{
  \code{memory.size} reports the current or maximum memory allocation of
  the \code{malloc} function used in this version of \R.

  \code{memory.limit} reports the limit in force on the total allocation.
}
\usage{
memory.size(max = FALSE)

memory.limit()
}
\arguments{
  \item{max}{logical. If true the maximum amount of memory obtained from
    the OS is reported, otherwise the amount currently in use.}
}
\details{
  Command-line flag \code{--max-mem-size} sets the maximum value of
  obtainable memory (including a very small amount of housekeeping
  overhead).
}
\value{
  Size in bytes.
}
\examples{
memory.size()
memory.size(TRUE)
round(memory.limit()/1048576.0, 2)
}
\keyword{utilities}
