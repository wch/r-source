\name{interactive}
\alias{interactive}
\title{Is R Running Interactively?}
\description{ 
  Return \code{TRUE} when \R is being used interactively and
  \code{FALSE} otherwise.
}
\usage{
interactive()
}
\seealso{
  \code{\link{source}},
  \code{\link{.First}}
}
\examples{
 .First <- function() if(interactive()) x11()
}
\keyword{environment}
\keyword{programming}
