\name{weekdays}
\alias{weekdays}
\alias{weekdays.POSIXt}
\alias{months}
\alias{months.POSIXt}
\alias{quarters}
\alias{quarters.POSIXt}
\alias{julian}
\alias{julian.POSIXt}
\title{Extract Parts of a POSIXt Object}
\description{
  Extract the weekday, month or quarter, or the Julian time
  (days since some origin).  These are generic functions: the methods
  for the internal date-time classes are documented here.
}
\usage{
\method{weekdays}{POSIXt}(x, abbreviate = FALSE)
\method{months}{POSIXt}(x, abbreviate = FALSE)
\method{quarters}{POSIXt}(x)
\method{julian}{POSIXt}(x, origin = as.POSIXct("1970-01-01", tz="GMT"))
}
\arguments{
  \item{x}{an object inheriting from class \code{"POSIXt"}.}
  \item{abbreviate}{logical.  Should the names be abbreviated?}
  \item{origin}{an length-one object inheriting from class \code{"POSIXt"}.}
}
\value{
  \code{weekdays} and \code{months} return a character
  vector of names in the locale in use.

  \code{quarters} returns a character vector of \code{"Q1"} to
  \code{"Q4"}.

  \code{julian} returns the number of days (possibly fractional)
  since the origin, with the origin as a \code{"origin"} attribute.
}
\note{
  Other components such as the day of the month or the year are
  very easy to compute: just use \code{\link{as.POSIXlt}} and extract the
  relevant component.
}

\seealso{\code{\link{DateTimeClasses}}}

\examples{
weekdays(.leap.seconds)
months(.leap.seconds)
quarters(.leap.seconds)
}
\keyword{chron}
