\name{difftime}
\alias{difftime}
\alias{as.difftime}
\alias{as.double.difftime}
\alias{print.difftime}
\alias{round.difftime}
\alias{format.difftime}
\alias{units.difftime}
\alias{units<-.difftime}
\alias{Ops.difftime}
\alias{*.difftime}
\alias{/.difftime}
\alias{Math.difftime}
\alias{Summary.difftime}
\alias{[.difftime}
\alias{mean.difftime}
\alias{units}
\alias{units<-}
\title{Time Intervals}
\description{
  Create, print and round time intervals.
}
\usage{
\special{time1 - time2}

difftime(time1, time2, tz = "",
         units = c("auto", "secs", "mins", "hours", "days", "weeks"))

as.difftime(tim, format = "\%X", units="auto")


\method{round}{difftime}(x, digits = 0, \dots)
\method{format}{difftime}(x, ...)
\method{units}{difftime}(x)
\method{units}{difftime}(x) <- value
\method{as.double}{difftime}(x, units="auto", ...)
}
\arguments{
  \item{time1, time2}{\link[base:DateTimeClasses]{date-time} or
    \link[base:Dates]{date} objects.}
  \item{tz}{a timezone specification to be used for the conversion.
    System-specific, but \code{""} is the current time zone, and
    \code{"GMT"} is UTC.}
  \item{units}{character.  Units in which the results are desired. Can
    be abbreviated.}
  \item{value}{character.  Like \code{units} above, except that
    abbreviations are not allowed.}
  \item{tim}{character string or numeric value specifying a time interval.}
  \item{format}{character specifying the format of \code{tim}: see
    \code{\link{strptime}}.  The default is a locale-specific time format.}
  \item{x}{an object inheriting from class \code{"difftime"}.}
  \item{digits}{integer.  Number of significant digits to retain.}
  \item{\dots}{arguments to be passed to or from other methods.}
}
\details{
  Function \code{difftime} calculates a difference of two date/time objects
  and returns an object of class \code{"difftime"}
  with an attribute indicating the units.  There is a \code{\link{round}}
  method for objects of this class, as well as methods for the
  group-generic (see \code{\link[base:groupGeneric]{Ops}}) logical and
  arithmetic operations.

  If \code{units = "auto"}, a suitable set of units is chosen, the largest
  possible (excluding \code{"weeks"}) in which all the absolute
  differences are greater than one.

  Subtraction of date-time objects gives an object of this class,
  by calling \code{difftime} with \code{units="auto"}.  Alternatively,
  \code{as.difftime()} works on character-coded or numeric time
  intervals; in the latter case, units must be specified, and
  \code{format} has no effect.

  Limited arithmetic is available on \code{"difftime"} objects: they can
  be added or subtracted, and multiplied or divided by a numeric vector.
  In addition, adding or subtracting a numeric vector implicitly
  converts the numeric vector to a \code{"difftime"} object with the
  same units as the \code{"difftime"} object.

  The units of a \code{"difftime"} object can be extracted by the
  \code{units} function, which also has an replacement form.  If the units
  are changed, the numerical value is scaled accordingly.

  The \code{as.double} method returns the numeric value expressed in
  the specified units.  Using  \code{units="auto"} means the units of the
  object.
  
  The \code{format} method simply formats the numeric value and appends
  the units as a text string.
}
\seealso{
  \code{\link{DateTimeClasses}}.
}
\examples{
(z <- Sys.time() - 3600)
Sys.time() - z                # just over 3600 seconds.

## time interval between releases of R 1.2.2 and 1.2.3.
ISOdate(2001, 4, 26) - ISOdate(2001, 2, 26)

as.difftime(c("0:3:20", "11:23:15"))
as.difftime(c("3:20", "23:15", "2:"), format= "\%H:\%M")# 3rd gives NA
(z <- as.difftime(c(0,30,60), units="mins"))
as.numeric(z, units="secs")
as.numeric(z, units="hours")
format(z)
}
\keyword{utilities}
\keyword{chron}
