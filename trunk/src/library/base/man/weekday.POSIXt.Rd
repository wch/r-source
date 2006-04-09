\name{weekdays}
\alias{weekdays}
\alias{weekdays.POSIXt}
\alias{weekdays.Date}
\alias{months}
\alias{months.POSIXt}
\alias{months.Date}
\alias{quarters}
\alias{quarters.POSIXt}
\alias{quarters.Date}
\alias{julian}
\alias{julian.POSIXt}
\alias{julian.Date}
\title{Extract Parts of a POSIXt or Date Object}
\description{
  Extract the weekday, month or quarter, or the Julian time
  (days since some origin).  These are generic functions: the methods
  for the internal date-time classes are documented here.
}
\usage{
weekdays(x, abbreviate)
\method{weekdays}{POSIXt}(x, abbreviate = FALSE)
\method{weekdays}{Date}(x, abbreviate = FALSE)

months(x, abbreviate)
\method{months}{POSIXt}(x, abbreviate = FALSE)
\method{months}{Date}(x, abbreviate = FALSE)

quarters(x, abbreviate)
\method{quarters}{POSIXt}(x, \dots)
\method{quarters}{Date}(x, \dots)

julian(x, \dots)
\method{julian}{POSIXt}(x, origin = as.POSIXct("1970-01-01", tz="GMT"), \dots)
\method{julian}{Date}(x, origin = as.Date("1970-01-01"), \dots)
}
\arguments{
  \item{x}{an object inheriting from class \code{"POSIXt"} or \code{"Date"}.}
  \item{abbreviate}{logical.  Should the names be abbreviated?}
  \item{origin}{an length-one object inheriting from class
    \code{"POSIXt"} or \code{"Date"}.}
  \item{\dots}{arguments for other methods.}
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
  very easy to compute: just use \code{\link{as.POSIXlt}} and extract
  the relevant component.
}

\seealso{
  \code{\link{DateTimeClasses}}, \code{\link{Date}}
}

\examples{
weekdays(.leap.seconds)
months(.leap.seconds)
quarters(.leap.seconds)
}
\keyword{chron}
