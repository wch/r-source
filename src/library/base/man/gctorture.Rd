\name{gctorture}
\title{Torture garbage collector}
\usage{
gctorture(on=TRUE)
}
\alias{gctorture}
\arguments{
 \item{on}{ Turning it on/off }
}
\description{
 Provokes garbage collection on (nearly) every memory allocation.
Intended to ferret out memory protection bugs. Also makes R run
\emph{very} slowly, unfortunately.}

\value{
	None
}
\author{ Peter Dalgaard }

\keyword{ ~keyword }%-- one or more ...
