\name{readline}
\title{Read a Line from the Terminal}
\usage{
readline()
}
\alias{readline}
\description{
  \code{readline} reads a line from the terminal, returning it as a
  character string.
}
\examples{
fun <- function() {
  cat("Are you a satisfied R user? ")
  ANSWER <- readline()
  if (substr(ANSWER, 1, 1) == "n")
    cat("This is impossible.  YOU LIED!\n")
  else
    cat("I knew it.\n")
}
fun()
}
\keyword{utilities}
