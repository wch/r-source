% File src/library/base/man/seq.Date.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
% Distributed under GPL 2 or later

\name{seq.Date}
\alias{seq.Date}
\title{Generate Regular Sequences of Dates}
\description{
  The method for \code{\link{seq}} for objects of class
  \code{"\link{Date}"} representing calendar dates.
}
\usage{
\method{seq}{Date}(from, to, by, length.out = NULL, along.with = NULL, \dots)
}
\arguments{
  \item{from}{starting date; optional.}
  \item{to}{end date; optional.}
  \item{by}{increment of the sequence; optional.  See \sQuote{Details}.}
  \item{length.out}{integer, optional.  Desired length of the sequence.}
  \item{along.with}{optional; take the length from the length of this argument.}
  \item{\dots}{arguments passed to or from other methods.}
}
\details{
  \code{by} can be specified in several ways.
  \itemize{
    \item A number, taken to be in days.
    \item A object of class \code{\link{difftime}}
    \item A character string, containing one of \code{"day"},
    \code{"week"}, \code{"month"}, \code{"quarter"} or \code{"year"}, or a
    \code{\link{pmatch}()}able abbreviation of these.
    This can optionally be preceded by a (positive or negative) integer
    and a space, or followed by \code{"s"}.

    See \code{\link{seq.POSIXt}} for the details of \code{"month"}.

    In the case \code{seq(from, to)}, the default for \code{by} is
    \code{"day"} (or \code{"1 day"}).
  }
}
\value{
  A vector of class \code{"Date"}.
  Type \code{"\link{integer}"} is often preserved on purpose, saving
  \code{\link{object.size}}.
}
\seealso{\code{\link{Date}}}

\examples{
## first days of years
seq(as.Date("1910/1/1"), as.Date("1999/1/1"), "years")
## by month
seq(as.Date("2000/1/1"), by = "month", length.out = 12)
## quarters
seq(as.Date("2000/1/1"), as.Date("2003/1/1"), by = "quarter")

## 3-week period ending on a fixed date
seq(to = as.Date("2024-06-18"), by = "day", length.out = 21)

## find all 7th of the month _strictly_ inside two dates, the last being a 7th.
st <- as.Date("1998-12-17")
en <- as.Date("2000-1-7")
ll <- seq(en, st, by = "-1 month")
rev(ll[st < ll & ll < en])

## can abbreviate 'month' to 'm':
identical(seq(st, en, by = "m"),
          seq(st, en, by = "1 month"))
}
\keyword{manip}
\keyword{chron}
