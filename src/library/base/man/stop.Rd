\name{stop}
\title{Stop Function Execution}
\usage{
stop(message)
geterrmessage()
}
\alias{stop}
\alias{geterrmessage}
\arguments{
  \item{message}{a character vector (of length 1) or \code{NULL}.}
}
\description{
  \code{stop} stops execution of the current expression, prints the
  message given as its argument, then executes an error action.

  \code{geterrmessage} gives the last error message.
}
\details{
  The error action is controlled by the current error handler set by
  \code{options(error=)}. The default behaviour  (the \code{NULL}
  error-handler) in interactive use is
  to return to the top level prompt, and in non-interactive use to
  (effectively) call \code{q("no", status=1, runLast=FALSE}).
}
\value{
  \code{geterrmessage} gives the last error message, as character string
  ending in \code{"\n"}.
}
\seealso{
  \code{\link{warning}}, \code{\link{restart}} to catch errors and retry,
  and  \code{\link{options}} for setting error handlers.
}
\examples{
\testonly{iter <- 9 #-don't want a stop(.) below!}
if(iter > 10) stop("too many iterations")
}
\keyword{environment}
\keyword{programming}
