\name{gc}
\title{Garbage Collection}
\usage{
gc()
gcinfo(verbose)
}
\alias{gc}
\alias{gcinfo}
\arguments{
\item{verbose}{a logical value.}
}
\value{
A call of \code{gc} causes a garbage collection to take place.
This usually takes place automagically without user intervention.

The associated function \code{gcinfo} sets a flag so that
automatic collection is either silent (\code{verbose=FALSE}) or
prints memory use statistics (\code{verbose=TRUE}).
}
\examples{
gc() #- do it now
gcinfo(TRUE) #-- in the future, show when you do it
}
\keyword{environment}
