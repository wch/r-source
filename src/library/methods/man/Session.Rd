\name{Session}
\alias{sessionData}
\alias{traceOn}
\alias{traceOff}
\alias{browseAll}
\title{Deprecated:  Session Data and Debugging Tools}
\description{
  The functions \code{traceOn} and \code{traceOff} have been replaced by
  extended versions of the functions \code{\link{trace}} and
  \code{\link{untrace}}, and should not be used.
}
\usage{
sessionData()

traceOn(what, tracer=browseAll, exit=NULL)

traceOff(what)

browseAll()

}
\details{
  \describe{
    \item{\code{sessionData}:}{
      return the index of the session data in the search list,
      attaching it if it is not attached.
    }

    \item{\code{traceOn}:}{
      initialize tracing on calls to function \code{what}.  The function
      or expression \code{tracer} is called on entry, and the function
      or expression \code{exit} on exit.
    }
  
    \item{\code{traceOff}:}{
      turn off tracing of this function.
    }
  
    \item{\code{browseAll}:}{ 
      browse the current stack of function calls.

      Uses the function \code{debugger} to set up browser calls on the
      frames.  On exit from that function, computation continues after
      the call to \code{browseAll}.  Computations done in the frames
      will have no effect.
    }
  }
}
\references{
  See \emph{Programming with Data} (John M. Chambers, Springer, 1998)
  for the equivalent functions.
}
\keyword{internal}
