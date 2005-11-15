\name{as.POSIX*}
\alias{as.POSIXct}
\alias{as.POSIXct.default}
\alias{as.POSIXct.POSIXlt}
\alias{as.POSIXct.date}
\alias{as.POSIXct.dates}
\alias{as.POSIXct.Date}
\alias{as.POSIXlt}
\alias{as.numeric.POSIXlt}

\title{Date-time Conversion Functions}
\description{
  Functions to manipulate objects of classes \code{"POSIXlt"} and
  \code{"POSIXct"} representing calendar dates and times (to the nearest
  second). 
}
\usage{
as.POSIXct(x, tz = "")
as.POSIXlt(x, tz = "")

\method{as.numeric}{POSIXlt}(x)
}
\arguments{
  \item{x}{An object to be converted.}
  \item{tz}{A timezone specification to be used for the conversion,
    \emph{if one is required}.  System-specific, but \code{""}
    is the current timezone, and \code{"GMT"} is UTC
    (Coordinated Universal Time, in French).}
}
\details{
  The \code{as.POSIX*} functions convert an object to one of the two
  classes used to represent date/times (calendar dates plus time to the
  nearest second).  They can convert a wide variety of objects,
  including objects of the other class and of classes \code{"Date"},
  \code{"date"} (from package \pkg{\link[date:as.date]{date}} or
  \pkg{\link[survival:as.date]{survival}}), \code{"chron"} and
  \code{"dates"} (from package \pkg{\link[chron]{chron}}) to these
  classes.  Dates are treated as being at midnight UTC.

  They can also convert character strings of the formats
  \code{"2001-02-03"} and \code{"2001/02/03"} optionally followed by
  white space and a time in the format \code{"14:52"} or
  \code{"14:52:03"}.  (Formats such as \code{"01/02/03"} are ambiguous
  but can be converted via a format specification by
  \code{\link{strptime}}.)

  Logical \code{NA}s can be converted to either of the classes, but no
  other logical vectors can be.

  The \code{as.numeric} method converts \code{"POSIXlt"} objects to
  \code{"POSIXct"}.
}
\value{
  \code{as.POSIXct} and \code{as.POSIXlt} return an object of the
  appropriate class.  If \code{tz} was specified, \code{as.POSIXlt}
  will give an appropriate \code{"tzone"} attribute.
}
\note{
  If you want to extract specific aspects of a time (such as the day of
  the week) just convert it to class \code{"POSIXlt"} and extract the
  relevant component(s) of the list, or if you want a character
  representation (such as a named day of the week) use
  \code{\link{format.POSIXlt}} or \code{\link{format.POSIXct}}.

  If a timezone is needed and that specified is invalid on your system,
  what happens is system-specific but it will probably be ignored. 
}
\seealso{
  \link{DateTimeClasses} for details of the classes;
  \code{\link{strptime}} for conversion to and from character
  representations.
}
\examples{
(z <- Sys.time())             # the current date, as class "POSIXct"
unclass(z)                    # a large integer
floor(unclass(z)/86400)       # the number of days since 1970-01-01
(z <- as.POSIXlt(Sys.time())) # the current date, as class "POSIXlt"
unlist(unclass(z))            # a list shown as a named vector

as.POSIXlt(Sys.time(), "GMT") # the current time in GMT
}
\keyword{utilities}
\keyword{chron}
