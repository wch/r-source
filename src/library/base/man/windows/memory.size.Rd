\name{memory.size}
\alias{memory.size}
\title{Report on Memory Allocation}
\description{
  This function reports the current or maximum memory allocation of
  the \code{malloc} function used in this version of \R.
}
\usage{
memory.size(max = FALSE)
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
}
\keyword{utilities}
