\name{difftime}
\alias{difftime}
\alias{as.difftime}
\alias{print.difftime}
\alias{round.difftime}
\alias{Ops.difftime}
\alias{*.difftime}
\alias{/.difftime}
\alias{Math.difftime}
\alias{Summary.difftime}
\alias{[.difftime}
\alias{mean.difftime}
\title{Time Intervals}
\description{
  Create, print and round time intervals.
}
\usage{
time1 - time2

difftime(time1, time2, tz = "",
         units = c("auto", "secs", "mins", "hours", "days", "weeks"))

as.difftime(tim, format = "\%X")

\method{round}{difftime}(x, digits = 0)
}
\arguments{
  \item{time1, time2}{date-time objects.}
  \item{tz}{a timezone specification to be used for the conversion.
    System-specific, but \code{""} is the current time zone, and
    \code{"GMT"} is UTC.}
  \item{units}{character.  Units in which the results are desired. Can
    be abbreviated.}
  \item{tim}{character string specifying a time interval.}
  \item{format}{character specifying the format of \code{tim}.}
  \item{x}{an object inheriting from class \code{"difftime"}.}
  \item{digits}{integer.  Number of significant digits to retain.}
}
\details{
  Function \code{difftime} takes a difference of two date/time objects
  (of either class) and returns an object of class \code{"difftime"}
  with an attribute indicating the units.  There is a \code{\link{round}}
  method for objects of this class, as well as methods for the
  group-generic (see \code{\link[base]{Ops}}) logical and
  arithmetic operations.

  If \code{units = "auto"}, a suitable set of units is chosen, the largest
  possible (excluding \code{"weeks"}) in which all the absolute
  differences are greater than one.

  Subtraction of two date-time objects gives an object of this class,
  by calling \code{difftime} with \code{units="auto"}.  Alternatively,
  \code{as.difftime()} works on character-coded time intervals.

  Limited arithmetic is available on \code{"difftime"} objects: they can
  be added or subtracted, and multiplied or divided by a numeric vector.
  In addition, adding or subtracting a numeric vector implicitly
  converts the numeric vector to a \code{"difftime"} object with the
  same units as the \code{"difftime"} object.
}
\seealso{
  \code{\link{DateTimeClasses}}.
}
\examples{
(z <- Sys.time() - 3600)
Sys.time() - z                # just over 3600 seconds.

## time interval between releases of 1.2.2 and 1.2.3.
ISOdate(2001, 4, 26) - ISOdate(2001, 2, 26)

as.difftime(c("0:3:20", "11:23:15"))
as.difftime(c("3:20", "23:15", "2:"), format= "\%H:\%M")# 3rd gives NA
}
\keyword{utilities}
\keyword{chron}
