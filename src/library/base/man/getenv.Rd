\name{getenv}
\title{Get Environment Variables}
\usage{
getenv(x)
}
\alias{getenv}
\arguments{
  \item{x}{a character vector}
}
\description{
  \code{getenv} obtains the values of the environment variables named by
  \code{x}.
}
\value{
  A vector of the same length as \code{x}, with the variable names as
  its \code{names} attribute.  Each element holds the value of the
  environment variable named by the corresponding component of \code{x}
  (or \code{""} if no environment variable with that name was found).
}
\examples{
getenv(c("R_HOME", "R_PAPERSIZE", "R_PRINTCMD"))
}
\keyword{environment}
\keyword{utilities}
