\name{cut.POSIXt}
\alias{cut.POSIXt}
\title{Convert a Date-Time Object to a Factor}
\description{
  Method for \code{\link{cut}} applied to date-time objects.
}
\usage{
\method{cut}{POSIXt}(x, breaks, labels = NULL, start.on.monday = TRUE,
    right = FALSE, \dots)
}
\arguments{
  \item{x}{an object inheriting from class \code{"POSIXt"}.}
  \item{breaks}{
    a vector of cut points \emph{or} number giving the number of
    intervals which \code{x} is to be cut into \emph{or} an
    interval specification, one of \code{"sec"}, \code{"min"},
    \code{"hour"}, \code{"day"}, \code{"DSTday"}, \code{"week"},
    \code{"month"} or \code{"year"}, optionally preceded by an integer
    and a space, or followed by \code{"s"}.
  }
  \item{labels}{
    labels for the levels of the resulting category.  By default,
    labels are constructed from the left-hand end of the intervals
    (which are include for the default value of \code{right}). If
    \code{labels = FALSE}, simple integer codes are returned instead
    of a factor.
  }
  \item{start.on.monday}{logical. If \code{breaks = "weeks"}, should the
    week start on Mondays or Sundays?}
  \item{right, \dots}{arguments to be passed to or from other methods.}
}
\value{
     A factor is returned, unless \code{labels = FALSE} which returns
     the integer level codes.
}
\seealso{
  \code{\link{seq.POSIXt}}, \code{\link{cut}}
}
\examples{
## random dates in a 10-week period
cut(ISOdate(2001, 1, 1) + 70*86400*runif(100), "weeks")
}
\keyword{manip}
\keyword{chron}
