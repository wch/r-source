\name{Sys.putenv}
\title{Set Environment Variables}
\usage{
Sys.putenv(\dots)
}
\alias{Sys.putenv}
\arguments{
  \item{\dots}{arguments in \code{name=value} form, with \code{value}
    coercible to a character string.}
}
\description{
  \code{putenv} sets environment variables (for other processes called
  from within \R or future calls to \code{\link{Sys.getenv}} from this
  \R process).
}
\value{
  A logical vector of the same length as \code{x}, with elements being
  true if setting the corresponding variable succeeded.
}
\details{
  Non-standard \R names must be quoted: see the Examples section.
}
\note{
  Not all systems need support \code{Sys.putenv} (although all known
  current platforms do).
}
\seealso{
  \code{\link{Sys.getenv}}, \link{Startup} for ways to set environment
  variables for the \R session.
  
  \code{\link{setwd}} for the working directory.
}
\examples{
print(Sys.putenv(R_TEST="testit", "A+C"=123))
Sys.getenv("R_TEST")
}
\keyword{environment}
\keyword{utilities}
