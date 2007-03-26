\name{readline}
\title{Read a Line from the Terminal}
\concept{waiting for input}
\concept{user prompting}
\usage{
readline(prompt = "")
}
\alias{readline}
\arguments{
  \item{prompt}{the string printed when prompting the user for input.
  Should usually end with a space \code{" "}.}
}
\description{
  \code{readline} reads a line from the terminal
}
\details{
  The prompt string will be truncated to a maximum allowed length,
  normally 256 chars (but can be changed in the source code).
}
\value{
  A character vector of length one.
}
\seealso{ \code{\link{readLines}} for reading text lines of connections,
  including files.
}
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
