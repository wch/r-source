\name{getpid}
\alias{Sys.getpid}

\title{Get the Process ID of the R Session}
\description{
  Get the process ID of the \R Session.  It is guaranteed by the
  operating system that two \R sessions running simultaneously will
  have different IDs, but it is possible that \R sessions running at
  different times will have the same ID.
}
\usage{
Sys.getpid()
}
\value{
  An integer, usually a small integer between 0 and 32767 under
  Unix-alikes and a much small integer under Windows.
}
\examples{
Sys.getpid()
}
\keyword{utilities}
