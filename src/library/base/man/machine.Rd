\name{machine}
\title{Determine the Machine \R is Running On}
\usage{
machine()
}
\alias{machine}
\description{
  This function returns a string which specifies what kind of
  environment \R is being run in.
}
\examples{
machine()# to see yours
if (machine() == "Macintosh")
  cat("You are using a Macintosh computer\n")
}
\keyword{environment}
\keyword{programming}
