% File src/library/base/man/weekday.POSIXt.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2022 R Core Team
% Distributed under GPL 2 or later

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
\title{Extract Parts of a \I{POSIXt} or Date Object}
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
\method{julian}{POSIXt}(x, origin = as.POSIXct("1970-01-01", tz = "GMT"), \dots)
\method{julian}{Date}(x, origin = as.Date("1970-01-01"), \dots)
}
\arguments{
  \item{x}{an object inheriting from class \code{"POSIXt"} or \code{"Date"}.}
  \item{abbreviate}{logical vector (possibly recycled).  Should the names be
    abbreviated?}
  \item{origin}{an length-one object inheriting from class
    \code{"POSIXt"} or \code{"Date"}.}
  \item{\dots}{arguments for other methods.}
}
\value{
  \code{weekdays} and \code{months} return a character
  vector of names in the locale in use, i.e., \code{\link{Sys.getlocale}("LC_TIME")}.

  \code{quarters} returns a character vector of \code{"Q1"} to
  \code{"Q4"}.

  \code{julian} returns the number of days (possibly fractional)
  since the origin, with the origin as a \code{"origin"} attribute.
  All time calculations in \R are done ignoring leap-seconds.
}
\note{
  Other components such as the day of the month or the year are
  very easy to compute: just use \code{\link{as.POSIXlt}} and extract
  the relevant component.  Alternatively (especially if the components
  are desired as character strings), use \code{\link{strftime}}.
}

\seealso{
  \code{\link{DateTimeClasses}}, \code{\link{Date}};
  \code{\link{Sys.getlocale}("LC_TIME")} crucially for \code{months()} and \code{weekdays()}.
}

\examples{
\donttest{## first two are locale dependent:
weekdays(.leap.seconds)
months  (.leap.seconds)}
quarters(.leap.seconds)

## Show how easily you get month, day, year, day (of {month, week, yr}), ... :
## (remember to count from 0 (!): mon = 0..11, wday = 0..6,  etc !!)

##' Transform (Time-)Date vector  to  convenient data frame :
dt2df <- function(dt, dName = deparse(substitute(dt))) {
    DF <- as.data.frame(unclass(as.POSIXlt( dt )))
    `names<-`(cbind(dt, DF, deparse.level=0L), c(dName, names(DF)))
}
## e.g.,
dt2df(.leap.seconds)    # date+time
\donttest{dt2df(Sys.Date() + 0:9) # date
}
##' Even simpler:  Date -> Matrix - dropping time info {sec,min,hour, isdst}
d2mat <- function(x) simplify2array(unclass(as.POSIXlt(x))[4:7])
## e.g.,
d2mat(seq(as.Date("2000-02-02"), by=1, length.out=30)) # has R 1.0.0's release date

\donttest{
## Julian Day Number (JDN, https://en.wikipedia.org/wiki/Julian_day)
## is the number of days since noon UTC on the first day of 4317 BCE.
## in the proleptic Julian calendar.  To more recently, in
## 'Terrestrial Time' which differs from UTC by a few seconds
## See https://en.wikipedia.org/wiki/Terrestrial_Time
julian(Sys.Date(), -2440588) # from a day
floor(as.numeric(julian(Sys.time())) + 2440587.5) # from a date-time
}
}
\keyword{chron}
