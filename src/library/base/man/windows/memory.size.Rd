\name{memory.size}
\alias{memory.size}
\title{Report on Memory Allocation}
\description{
  This function reports on the current or maximum memory allocation of
  the \code{malloc} function used in this version of \R.
}
\usage{
memory.size(max = FALSE)
}

\arguments{
  \item{max}{logical. If true the maximum amount of memory obtained from
    the OS is reported, otherwise the amount currently in use.}
}

\value{
  Size in bytes.
}
\examples{
memory.size()
memory.size(TRUE)
}
\keyword{utility}
