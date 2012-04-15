% File src/library/methods/man/callGeneric.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{callGeneric}
\alias{callGeneric}
\title{Call the Current Generic Function from a Method}
\description{
  A call to \code{callGeneric} can only appear inside a method
  definition.  It then results in a call to the current generic
  function.  The value of that call is the value of \code{callGeneric}.
  While it can be called from any method, it is useful and typically
  used in methods for group generic functions.
}
\usage{
callGeneric(...)
}
\arguments{
  \item{\dots}{
    Optionally, the arguments to the function in its next call.

    If no arguments are included in the call to \code{callGeneric}, the
    effect is to call the function with the current arguments.
    See the detailed description for what this really means.
  }
}
\details{
  The name and package of the current generic function is stored in the
  environment of the method definition object.  This name is looked up
  and the corresponding function called.

  The statement that passing no arguments to \code{callGeneric} causes
  the generic  function to be called with the current arguments is
  more precisely as follows.  Arguments that were missing in the current
  call are still missing (remember that \code{"missing"} is a valid
  class in a method signature).  For a formal argument, say \code{x}, that
  appears in the original call, there is a corresponding argument in the
  generated call equivalent to \code{x = x}.  In effect, this
  means that the generic function sees the same actual arguments, but
  arguments are evaluated only once.

  Using \code{callGeneric} with no arguments is prone to creating
  infinite recursion, unless one of the arguments in the signature has
  been modified in the current method so that a different method is selected.
}
\value{
  The value returned by the new call.
}
\references{
 Chambers, John M. (2008)
 \emph{Software for Data Analysis: Programming with R}
  Springer.  (For the R version.)

 Chambers, John M. (1998)
 \emph{Programming with Data}
 Springer (For the original S4 version.)
}
\seealso{\code{\link{GroupGenericFunctions}} for other information
  about group generic functions; \link{Methods} for the general behavior
  of method dispatch
}
\examples{
## the method for group generic function Ops
## for signature( e1="structure", e2="vector")
function (e1, e2)
{
    value <- callGeneric(e1@.Data, e2)
    if (length(value) == length(e1)) {
        e1@.Data <- value
        e1
    }
    else value
}

## For more examples
\dontrun{
showMethods("Ops", includeDefs = TRUE)
}

}

\keyword{programming}
\keyword{classes}
\keyword{methods}
