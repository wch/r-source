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
\description{
    A call of \code{gc} causes a garbage collection to take place.
    \code{gcinfo} sets a flag so that
    automatic collection is either silent (\code{verbose=FALSE}) or
    prints memory use statistics (\code{verbose=TRUE}).
}
\details{
A call of \code{gc} causes a garbage collection to take place.
This takes place automagically without user intervention, and the
primary purpose of calling \code{gc} is for the report on memory usage.
}
\value{
\code{gc} returns a 2*3 matrix with rows \code{"Ncells"} (\emph{cons
    cells}, usually 20 bytes each on 32-bit systems, 36 bytes each on
64-bit systems) and \code{"Vcells"} (\emph{vector cells}, 8 bytes each),
and columns \code{"free"}, \code{"total"} and \code{"(Mb)"} (rounded up
to the next 0.1Mb).

\code{gcinfo} returns the previous value of the flag.
}
\seealso{\code{\link{Memory}} on \R's memory management  and
    \code{\link{gctorture}} if you are an \R hacker.}
\examples{
gc() #- do it now
gcinfo(TRUE) #-- in the future, show when R does it
x <- integer(0); for(i in 1:18) x <- c(x,i,x)
gcinfo(verbose = FALSE)#-- don't show it anymore

( rgc <- gc(TRUE) )
(100*rgc[,"free"])\%/\% rgc[,"total"] # the percentages
rgc[2,"free"]/ (1024 / 8) # free vector heap in K bytes
}
\keyword{environment}
