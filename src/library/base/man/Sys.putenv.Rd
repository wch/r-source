\name{Sys.putenv}
\alias{Sys.putenv}
\alias{Sys.unsetenv}
\title{Set or Unset Environment Variables}
\usage{
Sys.putenv(\dots)

Sys.unsetenv(x)
}
\arguments{
  \item{\dots}{named arguments with values coercible to a character string.}
  \item{x}{a character vector, or an object coercible to character.}
}
\description{
  \code{Sys.putenv} sets environment variables (for other processes called
  from within \R or future calls to \code{\link{Sys.getenv}} from this
  \R process).

  \code{Sys.unsetenv} removes environment variables.
}
\value{
  A logical vector, with elements being true if (un)setting the
  corresponding variable succeeded.
}
\details{
  Non-standard \R names must be quoted in \code{Sys.putenv}:
  see the examples.
}
\note{
  Not all systems need support \code{Sys.putenv} (although all known
  current platforms do) nor \code{Sys.unsetenv}.
}
\seealso{
  \code{\link{Sys.getenv}}, \link{Startup} for ways to set environment
  variables for the \R session.
  
  \code{\link{setwd}} for the working directory.
}
\examples{
print(Sys.putenv(R_TEST="testit", "A+C"=123))
Sys.getenv("R_TEST")
#ifdef windows
Sys.unsetenv("R_TEST")
#endif
#ifdef unix
try(Sys.unsetenv("R_TEST"))  # may not be implemented
#endif
Sys.getenv("R_TEST", NA)
}
\keyword{environment}
\keyword{utilities}
