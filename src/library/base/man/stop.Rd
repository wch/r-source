\name{stop}
\title{Stop Function Execution}
\usage{
stop(message)
}
\alias{stop}
\description{
\code{stop} stops execution of the current expression, prints the
message given as its argument and then returns to the top level
prompt.
}
\seealso{
\code{\link{warning}}.
}
\examples{
\testonly{iter <- 9 #-don't want a stop(.) below!}
if(iter > 10) stop("too many iterations")
}
\keyword{environment}
\keyword{programming}
