\name{gctorture}
\title{Torture Garbage Collector}
\usage{
gctorture(on = TRUE)
}
\alias{gctorture}
\arguments{
  \item{on}{logical; turning it on/off.}
}
\description{
  Provokes garbage collection on (nearly) every memory allocation.
  Intended to ferret out memory protection bugs.  Also makes \R run
  \emph{very} slowly, unfortunately.
}
\value{
  Previous value.
}
\author{Peter Dalgaard}
\keyword{environment}

