\name{interactive}
\title{Is R Running Interactively?}
\usage{
interactive()
}
\alias{interactive}
\value{
This function returns \code{TRUE} when \R is being used interactively
and \code{FALSE} when not.
}
\seealso{
\code{\link{source}}, \code{\link{.First}}
}
\examples{
 .First <- function() if(interactive()) x11()
}
\keyword{environment}
\keyword{programming}
