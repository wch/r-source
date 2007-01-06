\name{Sys.getenv}
\title{Get Environment Variables}
\usage{
Sys.getenv(x = NULL, empty = "")
}
\alias{Sys.getenv}
\arguments{
  \item{x}{a character vector, or \code{NULL}.}
  \item{empty}{a character string.}
}
\description{
  \code{Sys.getenv} obtains the values of the environment variables.
}
\details{
  Both arguments will be coerced to character if necessary.

  Setting \code{empty = NA} will enable unset variables and those set to
  the value \code{""} to be distinguished, \emph{if the OS does}.
}
\value{
  A vector of the same length as \code{x}, with the variable names as
  its \code{names} attribute.  Each element holds the value of the
  environment variable named by the corresponding component of \code{x}
  (or the value of \code{empty} if no environment variable with that
  name was found).

  On most platforms \code{Sys.getenv()} will return a named vector giving
  the values of all the environment variables.
}
\seealso{
  \code{\link{Sys.putenv}},
  \code{\link{Sys.getlocale}} for the locale \dQuote{environment},
  \code{\link{getwd}} for the working directory.
}
\examples{
## whether HOST is set will be shell-dependent e.g. Solaris' csh does not.
Sys.getenv(c("R_HOME", "R_PAPERSIZE", "R_PRINTCMD", "HOST"))

str(s <- Sys.getenv()) # all settings (rather do not print)

## Language and Locale settings -- but rather use Sys.getlocale()
s[grep("^L(C|ANG)", names(s))]
}
\keyword{environment}
\keyword{utilities}
