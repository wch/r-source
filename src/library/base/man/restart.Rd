\name{restart}
\title{Restart an Expression}
\usage{
restart(on = TRUE)
}
\arguments{
\item{on}{if true a jump point is set; if false the jump point is removed}
}
\alias{restart}
\description{
  \code{restart} performs a type of non-local return.
}
\details{
  When \code{restart} is called with \code{on = TRUE} the evaluator marks that 
  function as a return point. Any errors or signals (such as control-C on
  Unix) cause control to return to the start of the function containing the
  call to \code{restart}. The most recently established function is always
  entered first.
}
\note{
  Use of this code is likely to result in an infinite loop. It should be
  avoided if possible and is included in \R for compatibility with S. The
  planned \R exception handling should remove all need for such a function.
}% and we have this, and restart is still here!  BDR 99/12/14

\examples{
f <- function(x, first = TRUE)
{
     restart(first)
     if( first ) {
        first <- FALSE
        return(log(x))
     }
     else
        return(NA)
}
f("a")
}
\keyword{programming}
