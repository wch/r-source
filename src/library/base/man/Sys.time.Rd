\name{Sys.time}
\alias{Sys.time}
\alias{Sys.timezone}

\title{Get Current Time and Timezone}
\description{
  \code{Sys.time} returns the system's idea of the current time and
  \code{Sys.timezone} returns the current time zone.
}
\usage{
Sys.time()
Sys.timezone()
}
\value{
  \code{Sys.time} returns an object of class \code{"POSIXct"} (see
  \link{DateTimeClasses}).

  \code{Sys.timezone} returns an OS-specific character string, possibly
  an empty string.
}
\seealso{
  \code{\link{date}} for the system time in a fixed-format character
  string.
}
\examples{
Sys.time()
## locale-specific version of date()
format(Sys.time(), "\%a \%b \%d \%X \%Y")

Sys.timezone()
}
\keyword{utilities}
\keyword{chron}
