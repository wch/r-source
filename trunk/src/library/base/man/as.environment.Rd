\name{as.environment}
\alias{as.environment}
\title{ Coerce to an Environment Object }
\description{
  Converts a number or a character string to the corresponding
  environment on the search path.
}
\usage{
as.environment(object)
}
\arguments{
  \item{object}{the object to convert.  If it is already an
    environment, just return it.  If it is a number, return the
    environment corresponding to that position on the search list.  If
    it is a character string, match the string to the names on the
    search list.}
}
\value{
  The corresponding environment object.
}
\author{ John Chambers }
\seealso{\code{\link{environment}} for creation and manipulation,
  \code{\link{search}}.}
\examples{
as.environment(1) ## the global environment
identical(globalenv(), as.environment(1)) ## is TRUE
try(as.environment("package:stats"))      ## stats need not be loaded
}
\keyword{data}
\keyword{environment}
