\name{gc}
\title{Garbage Collection}
\usage{
gc(verbose = .Options$verbose)
gcinfo(verbose)
}
\alias{gc}
\alias{gcinfo}
\arguments{
\item{verbose}{logical; if \code{TRUE}, the garbage collection prints
    statistics about cons cells [in Bytes] and the vector heap [in KBytes].}
}
\value{
A call of \code{gc} causes a garbage collection to take place.
This usually takes place automagically without user intervention.
\code{gc} returns a  2*2 matrix giving

The associated function \code{gcinfo} sets a flag so that
automatic collection is either silent (\code{verbose=FALSE}) or
prints memory use statistics (\code{verbose=TRUE}).
}
\examples{
gc() #- do it now
gcinfo(TRUE) #-- in the future, show when you do it
x _ integer(0); for(i in 1:18) x _ c(x,i,x)
gcinfo(verbose = FALSE)#-- don't show it anymore

( rgc <- gc(TRUE) )
(100*rgc[,"free"])\%/\% rgc[,"total"] # the percentages
rgc[2,"free"]/ (1024 / 8) # free vector heap in K bytes
}
\keyword{environment}
