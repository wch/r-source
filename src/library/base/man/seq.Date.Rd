% File src/library/base/man/seq.Date.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{seq.Date}
\alias{seq.Date}
\title{Generate Regular Sequences of Dates}
\description{
  The method for \code{\link{seq}} for objects of class class
  \code{"\link{Date}"} representing calendar dates.
}
\usage{
\method{seq}{Date}(from, to, by, length.out = NULL, along.with = NULL, \dots)
}
\arguments{
  \item{from}{starting date.  Required}
  \item{to}{end date.  Optional.}
  \item{by}{increment of the sequence.  Optional.  See \sQuote{Details}.}
  \item{length.out}{integer, optional.  Desired length of the sequence.}
  \item{along.with}{take the length from the length of this argument.}
  \item{\dots}{arguments passed to or from other methods.}
}
\details{
  \code{by} can be specified in several ways.
  \itemize{
    \item A number, taken to be in days.
    \item A object of class \code{\link{difftime}}
    \item A character string, containing one of \code{"day"},
    \code{"week"}, \code{"month"}, \code{"quarter"} or \code{"year"}.
    This can optionally be preceded by a (positive or negative) integer
    and a space, or followed by \code{"s"}.

    See \code{\link{seq.POSIXt}} for the details of \code{"month"}.
  }
}
\value{
  A vector of class \code{"Date"}.
}
\seealso{\code{\link{Date}}}

\examples{
## first days of years
seq(as.Date("1910/1/1"), as.Date("1999/1/1"), "years")
## by month
seq(as.Date("2000/1/1"), by = "month", length.out = 12)
## quarters
seq(as.Date("2000/1/1"), as.Date("2003/1/1"), by = "quarter")

## find all 7th of the month between two dates, the last being a 7th.
st <- as.Date("1998-12-17")
en <- as.Date("2000-1-7")
ll <- seq(en, st, by = "-1 month")
rev(ll[ll > st & ll < en])
}
\keyword{manip}
\keyword{chron}
