\name{Session}
\alias{sessionData}
\alias{traceOn}
\alias{traceOff}
\alias{browseAll}
\title{Session Data and Debugging Tools}
\usage{
sessionData()

traceOn(what, tracer=browseAll, exit=NULL)

traceOff(what)

browseAll()

}

\description{
  The function \code{sessionData} provides a work-around in R for the absence of a
  session database (what you get in S with the argument \code{where=0}
  to functions such as \code{get} and \code{assign}).

  The functions \code{traceOn}, \code{traceOff} and \code{browseAll} are
  debugging tools with roughly similar functionality to \code{trace}, \code{untrace} and
      \code{browser} as described in the reference.  Calling
      \code{traceOn} creates a new version of a function with arbitrary
      code inserted at the beginning of the call and/or on exit.
      Calling \code{traceOff} undoes the change.  The function
      \code{browseAll} is a useful interactive call to insert for
      tracing.  It behaves basically like \code{\link{debugger}}.

Note:  use \code{traceOn} only for testing functions in a library,
\emph{not} for functions defined in the global environment.  Because
the session data is in position 2 on the search list, the temporary
versions stored there will not override functions in the global environment.

  A more
  satisfactory version of these  facilities is likely to be provided
  in the future.  Use the current versions but don't write code that
  depends on them.
}
\arguments{
  \item{what}{
    The object to be traced or untraced.
  }
  \item{tracer, exit}{
    The code to use at the entrance or exit trace.  Either a function
    (which will be called with no arguments) or an expression (typically
    constructed from \code{link{quote}} or \code{\link{substitute}}) to
    be evaluated.
    }
  }
\details{
  
  \code{sessionData}: 
  return the index of the session data in the search list,
attaching it if it is not attached

  
  \code{traceOn}: 
  initialize tracing on calls to function \code{what}.  The function or expression
\code{tracer} is called on entry, and the function or expression \code{exit} on exit.

  
  \code{traceOff}: 
  turn off tracing of this function

  
  \code{browseAll}: 
  browse the current stack of function calls.

Uses the function \code{debugger} to set up browser calls
on the frames.  On exit from that function, computation
continues after the call to browseAll.  Computations done
in the frames will have no effect.

}
\references{
See   \emph{Programming with Data} (John M. Chambers, Springer, 1998) for the equivalent
functions.
}
\author{
  John Chambers
}
\keyword{programming}
\keyword{classes}
\keyword{methods}
