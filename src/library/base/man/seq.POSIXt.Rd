\name{seq.POSIXt}
\alias{seq.POSIXt}
\title{Generate Regular Sequences of Dates}
\description{
  The method for \code{\link{seq}} for data-time classes.
}
\usage{
\method{seq}{POSIXt}(from, to, by, length.out=NULL, along.with=NULL, ...)
}
\arguments{
  \item{from}{starting date. Required}
  \item{to}{end date. Optional. If supplied must be after \code{from}.}
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
    \code{"min"}, \code{"hour"}, \code{"day"}, \code{"week"},
    \code{"month"} or \code{"year"}.  This can optionally be
    preceded by an integer and a space.
  }
}
\value{
  A vector of class \code{"POSIXct"}.
}
\seealso{\code{\link{DateTimeClasses}}}

\examples{
## first days of years in the (commonly used) 20th century
seq(ISOdate(1910,1,1), ISOdate(1999,1,1), "years")
## by month
seq(ISOdate(2000,1,1), by="month", length=12)
## quarters
seq(ISOdate(1990,1,1), ISOdate(2000,1,1), by="3 months")
}
\keyword{manip}
\keyword{chron}
