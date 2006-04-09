\name{seq.POSIXt}
\alias{seq.POSIXt}
\title{Generate Regular Sequences of Dates}
\description{
  The method for \code{\link{seq}} for date-time classes.
}
\usage{
\method{seq}{POSIXt}(from, to, by, length.out = NULL, along.with = NULL, \dots)
}
\arguments{
  \item{from}{starting date. Required.}
  \item{to}{end date. Optional.}
  \item{by}{increment of the sequence. Optional. See Details.}
  \item{length.out}{integer, optional. desired length of the sequence.}
  \item{along.with}{take the length from the length of this argument.}
  \item{\dots}{arguments passed to or from other methods.}
}
\details{
  \code{by} can be specified in several ways.
  \itemize{
    \item A number, taken to be in seconds.
    \item A object of class \code{\link{difftime}}
    \item A character string, containing one of \code{"sec"},
    \code{"min"}, \code{"hour"}, \code{"day"}, \code{"DSTday"},
    \code{"week"}, \code{"month"} or \code{"year"}.  This can optionally be
    preceded by a (positive or negative) integer and a space,
    or followed by \code{"s"}.
  }
  The difference between \code{"day"} and \code{"DSTday"} is that the
  former ignores changes to/from daylight savings time and the latter takes
  the same clock time each day.  (\code{"week"} ignores DST, but
  \code{"7 DSTdays"}) can be used as an alternative.  \code{"month"} and
  \code{"year"} allow for DST.)

  The timezone of the result is taken from \code{from}: remember than
  GMT does not have daylight savings time.
}
\value{
  A vector of class \code{"POSIXct"}.
}
\seealso{\code{\link{DateTimeClasses}}}

\examples{
## first days of years
seq(ISOdate(1910,1,1), ISOdate(1999,1,1), "years")
## by month
seq(ISOdate(2000,1,1), by = "month", length = 12)
## quarters
seq(ISOdate(1990,1,1), ISOdate(2000,1,1), by = "3 months")
## days vs DSTdays: use c() to lose the timezone.
seq(c(ISOdate(2000,3,20)), by = "day", length = 10)
seq(c(ISOdate(2000,3,20)), by = "DSTday", length = 10)
seq(c(ISOdate(2000,3,20)), by = "7 DSTdays", length = 4)
}
\keyword{manip}
\keyword{chron}
