\name{Dates}
\alias{Date}
\alias{Dates}
\alias{print.Date}
\alias{summary.Date}
\alias{+.Date}
\alias{-.Date}
\alias{Ops.Date}
\alias{Math.Date}
\alias{Summary.Date}
\alias{[.Date}
\alias{[<-.Date}
\alias{[[.Date}
\alias{as.data.frame.Date}
\alias{c.Date}
\alias{mean.Date}

\title{Date Class}
\description{
  Description of the class \code{"Date"} representing calendar dates. 
}
\details{
  Dates are represented as the number of days since 1970-01-01, with
  negative values for earlier dates.  They are always printed
  following the rules of the current Gregorian calendar, even though
  that calendar was not in use long ago (it was adopted in 1752 in
  Great Britain and its colonies).

  It is intended that the date should be an integer, but this is not
  enforced in the internal representation.  Fractional days will be
  ignored when printing.  It is possible to produce fractional days via
  the \code{mean} method or by adding or subtracting an object of class
  \code{"\link{difftime}"}.
}
\seealso{
  \code{\link{Sys.Date}} for the current date.
  
  \code{\link{format.Date}} for conversion to and from character strings.

  \code{\link{plot.Date}} and
  \code{\link{hist.Date}} for plotting.

  \code{\link{weekdays}} for convenience extraction functions.

  \code{\link{seq.Date}}, \code{\link{cut.Date}},
  \code{\link{round.Date}} for utility operations.

  \code{\link{DateTimeClasses}} for date-time classes.
}
\examples{
(today <- Sys.Date())
format(today, "\%d \%b \%Y")  # with month as a word
(tenweeks <- seq(today, len=10, by="1 week")) # next ten weeks
weekdays(today)
months(tenweeks)
as.Date(.leap.seconds)
}
\keyword{utilities}
\keyword{chron}
