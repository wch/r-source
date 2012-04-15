% File src/library/methods/man/Session.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

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
 Chambers, John M. (2008)
 \emph{Software for Data Analysis: Programming with R}
  Springer.  (For the R version.)

 Chambers, John M. (1998)
 \emph{Programming with Data}
 Springer (For the original S4 version.) 
}
\keyword{internal}
