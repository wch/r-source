% File src/library/base/man/cut.POSIXt.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{cut.POSIXt}
\alias{cut.POSIXt}
\alias{cut.Date}
\title{Convert a Date or Date-Time Object to a Factor}
\description{
  Method for \code{\link{cut}} applied to date-time objects.
}
\usage{
\method{cut}{POSIXt}(x, breaks, labels = NULL, start.on.monday = TRUE,
    right = FALSE, \dots)

\method{cut}{Date}(x, breaks, labels = NULL, start.on.monday = TRUE,
    right = FALSE, \dots)
}
\arguments{
  \item{x}{an object inheriting from class \code{"POSIXt"} or \code{"Date"}.}
  \item{breaks}{
    a vector of cut points \emph{or} number giving the number of
    intervals which \code{x} is to be cut into \emph{or} an
    interval specification, one of \code{"sec"}, \code{"min"},
    \code{"hour"}, \code{"day"}, \code{"DSTday"}, \code{"week"},
    \code{"month"}, \code{"quarter"} or \code{"year"}, optionally
    preceded by an integer and a space, or followed by \code{"s"}.
    For \code{"Date"} objects only \code{"day"}, \code{"week"},
    \code{"month"}, \code{"quarter"} and \code{"year"} are allowed.
  }
  \item{labels}{
    labels for the levels of the resulting category.  By default,
    labels are constructed from the left-hand end of the intervals
    (which are included for the default value of \code{right}).  If
    \code{labels = FALSE}, simple integer codes are returned instead
    of a factor.
  }
  \item{start.on.monday}{logical.  If \code{breaks = "weeks"}, should the
    week start on Mondays or Sundays?}
  \item{right, \dots}{arguments to be passed to or from other methods.}
}
\details{
  Using both \code{right = TRUE} and \code{include.lowest = TRUE}
  will include both ends of the range of dates.

  Using \code{breaks = "quarter"} will create intervals of 3 calendar
  months, with the intervals beginning on January 1, April 1,
  July 1 or October 1, based upon \code{min(x)} as appropriate.

  As vector \code{break} will be sorted before use: \code{labels} will
  then correspond to the sorted vector.  
}
\value{
  A factor is returned, unless \code{labels = FALSE} which returns
  the integer level codes.
}
\seealso{
  \code{\link{seq.POSIXt}}, \code{\link{seq.Date}}, \code{\link{cut}}
}
\examples{
## random dates in a 10-week period
cut(ISOdate(2001, 1, 1) + 70*86400*stats::runif(100), "weeks")
cut(as.Date("2001/1/1") + 70*stats::runif(100), "weeks")

# The standards all have midnight as the start of the day, but some
# people incorrectly interpret it at the end of the previous day ...
tm <- seq(as.POSIXct("2012-06-01 06:00"), by = "6 hours", length.out = 24)
aggregate(1:24, list(day = cut(tm, "days")), mean)
# and a version with midnight included in the previous day:
aggregate(1:24, list(day = cut(tm, "days", right = TRUE)), mean)
}
\keyword{manip}
\keyword{chron}
