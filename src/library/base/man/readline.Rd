\name{readline}
\title{Read a Line from the Terminal}
\usage{
readline(prompt="")
}
\alias{readline}
\arguments{
  \item{prompt}{the string printed when prompting the user for input.
  Should usually end with a space \code{" "}.}
}
\description{
  \code{readline} reads a line from the terminal, returning it as a
  character string.
}
\examples{
fun <- function() {
  ANSWER <- readline("Are you a satisfied R user? ")
  if (substr(ANSWER, 1, 1) == "n")
    cat("This is impossible.  YOU LIED!\n")
  else
    cat("I knew it.\n")
}
fun()
}
\keyword{utilities}
