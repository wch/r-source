\name{name}
\title{Variable Names}
\usage{
as.name(x)
is.name(x)
}
\alias{as.name}
\alias{is.name}
\description{
  \code{as.name} coerces its argument to be a name.  The argument must
  be of mode \code{"character"}.

  \code{is.name} returns \code{TRUE} or \code{FALSE} depending on
  whether its argument is a name or not.
}
\seealso{
  \code{\link{call}}, \code{\link{is.language}}. For the internal
  object mode, \code{\link{typeof}}.
}
\examples{
an <- as.name("arrg")
is.name(an) # TRUE
str(an)# symbol
}
\keyword{programming}
\keyword{attribute}
