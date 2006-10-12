\name{gc}
\title{Garbage Collection}
\usage{
gc(verbose = getOption("verbose"), reset=FALSE)
gcinfo(verbose)
}
\alias{gc}
\alias{gcinfo}
\arguments{
  \item{verbose}{logical; if \code{TRUE}, the garbage collection prints
    statistics about cons cells and the vector heap.}
  \item{reset}{logical; if \code{TRUE} the values for maximum space used
    are reset to the current values}
}
\description{
  A call of \code{gc} causes a garbage collection to take place.
  \code{gcinfo} sets a flag so that
  automatic collection is either silent (\code{verbose=FALSE}) or
  prints memory usage statistics (\code{verbose=TRUE}).
}
\details{
  A call of \code{gc} causes a garbage collection to take place.
  This takes place automatically without user intervention, and the
  primary purpose of calling \code{gc} is for the report on memory usage.

  However, it can be useful to call \code{gc} after a large object
  has been removed, as this may prompt \R to return memory to the
  operating system.
}

\value{
  \code{gc} returns a matrix with rows \code{"Ncells"} (\emph{cons
    cells}), usually 28 bytes each on 32-bit systems and 56 bytes on
  64-bit systems, and \code{"Vcells"} (\emph{vector cells}, 8 bytes
  each), and columns \code{"used"} and \code{"gc trigger"},
  each also interpreted in megabytes (rounded up to the next 0.1Mb).

  If maxima have been set for either \code{"Ncells"} or \code{"Vcells"},
  a fifth column is printed giving the current limits in Mb (with
  \code{NA} denoting no limit).

  The final two columns show the maximum space used since the last call
  to \code{gc(reset=TRUE)} (or since \R started).
  
  \code{gcinfo} returns the previous value of the flag.
}
\seealso{\code{\link{Memory}} on \R's memory management,
  and \code{\link{gctorture}} if you are an \R hacker.
  \code{\link{reg.finalizer}} for actions to happen upon garbage
  collection.
}
\examples{
gc() #- do it now
gcinfo(TRUE) #-- in the future, show when R does it
x <- integer(100000); for(i in 1:18) x <- c(x,i)
gcinfo(verbose = FALSE)#-- don't show it anymore

gc(TRUE)

gc(reset=TRUE)
}
\keyword{environment}
