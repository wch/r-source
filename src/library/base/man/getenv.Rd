\name{Sys.getenv}
\title{Get Environment Variables}
\usage{
Sys.getenv(x)
}
\alias{Sys.getenv}
\arguments{
  \item{x}{a character vector, or missing}
}
\description{
  \code{Sys.getenv} obtains the values of the environment variables named by
  \code{x}.
}
\value{
  A vector of the same length as \code{x}, with the variable names as
  its \code{names} attribute.  Each element holds the value of the
  environment variable named by the corresponding component of \code{x}
  (or \code{""} if no environment variable with that name was found).

  On most platforms \code{Sys.getenv()} will return a named vector giving
  the values of all the environment variables.
}
\seealso{
  \code{\link{Sys.putenv}},
  \code{\link{getwd}} for the working directory.
}
\examples{
## whether HOST is set will be shell-dependent e.g. Solaris' csh does not.
Sys.getenv(c("R_HOME", "R_PAPERSIZE", "R_PRINTCMD", "HOST"))
}
\keyword{environment}
\keyword{utilities}
