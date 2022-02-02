% File src/library/base/man/readline.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

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
  \code{readline} reads a line from the terminal (in interactive use).
}
\details{
  The prompt string will be truncated to a maximum allowed length,
  normally 256 chars (but can be changed in the source code).

  This can only be used in an \link{interactive} session.
}
\value{
  A character vector of length one.  Both leading and trailing
  spaces and tabs are stripped from the result.

  In non-\link{interactive} use the result is as if the response was
  RETURN and the value is \code{""}.
}
\seealso{
  \code{\link{readLines}} for reading text lines from connections,
  including files.
}
\examples{
fun <- function() {
  ANSWER <- readline("Are you a satisfied R user? ")
  ## a better version would check the answer less cursorily, and
  ## perhaps re-prompt
  if (substr(ANSWER, 1, 1) == "n")
    cat("This is impossible.  YOU LIED!\n")
  else
    cat("I knew it.\n")
}
if(interactive()) fun()
}
\keyword{utilities}
