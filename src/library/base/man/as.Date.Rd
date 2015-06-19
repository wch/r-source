% File src/library/base/man/as.Date.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{as.Date}
\alias{format.Date}
\alias{as.character.Date}
\alias{as.Date}
\alias{as.Date.character}
\alias{as.Date.default}
\alias{as.Date.factor}
\alias{as.Date.POSIXct}
\alias{as.Date.POSIXlt}
\alias{as.Date.date}
\alias{as.Date.dates}
\alias{as.Date.numeric}

\title{Date Conversion Functions to and from Character}
\description{
  Functions to convert between character representations and objects of
  class \code{"Date"} representing calendar dates.
}
\usage{
as.Date(x, \dots)
\method{as.Date}{character}(x, format = "", \dots)
\method{as.Date}{numeric}(x, origin, \dots)
\method{as.Date}{POSIXct}(x, tz = "UTC", \dots)

\method{format}{Date}(x, \dots)

\method{as.character}{Date}(x, \dots)
}
\arguments{
  \item{x}{An object to be converted.}
  \item{format}{A character string.  If not specified, it will try
    \code{"\%Y-\%m-\%d"} then \code{"\%Y/\%m/\%d"} on the first
    non-\code{NA} element, and give an error if neither works.}
  \item{origin}{a Date object, or something which can be coerced by
    \code{as.Date(origin, \dots)} to such an object.}
  \item{tz}{a time zone name.}
  \item{\dots}{Further arguments to be passed from or to other methods,
    including \code{format} for \code{as.character} and \code{as.Date}
    methods.}
}
\details{
  The usual vector re-cycling rules are applied to \code{x} and
  \code{format} so the answer will be of length that of the longer of the
  vectors.

  Locale-specific conversions to and from character strings are used
  where appropriate and available.  This affects the names of the days
  and months.

  The \code{as.Date} methods accept character strings, factors, logical
  \code{NA} and objects of classes \code{"\link{POSIXlt}"} and
  \code{"\link{POSIXct}"}.  (The last is converted to days by ignoring
  the time after midnight in the representation of the time in specified
  time zone, default UTC.)  Also objects of class \code{"date"} (from
  package \pkg{\link[date:as.date]{date}}) and \code{"dates"} (from
  package \pkg{\link[chron]{chron}}).  Character strings are processed
  as far as necessary for the format specified: any trailing characters
  are ignored.

  \code{as.Date} will accept numeric data (the number of days since an
  epoch), but \emph{only} if \code{origin} is supplied.

  The \code{format} and \code{as.character} methods ignore any
  fractional part of the date.
}
\value{
  The \code{format} and \code{as.character} methods return a character vector
  representing the date.  \code{NA} dates are returned as \code{NA_character_}.

  The \code{as.Date} methods return an object of class \code{"\link{Date}"}.
}
\note{
  The default formats follow the rules of the ISO 8601 international
  standard which expresses a day as \code{"2001-02-03"}.

  If the date string does not specify the date completely, the returned
  answer may be system-specific.  The most common behaviour is to assume
  that a missing year, month or day is the current one.  If it specifies
  a date incorrectly, reliable implementations will give an error and
  the date is reported as \code{NA}.  Unfortunately some common
  implementations (such as \samp{glibc}) are unreliable and guess at the
  intended meaning.

  Years before 1CE (aka 1AD) will probably not be handled correctly.
}
\section{Conversion from other Systems}{
  Most systems record dates internally as the number of days since some
  origin, but this is fraught with problems, including
  \itemize{
    \item Is the origin day 0 or day 1?  As the \sQuote{Examples} show,
    Excel manages to use both choices for its two date systems.
    \item If the origin is far enough back, the designers may show their
    ignorance of calendar systems.  For example, Excel's designer
    thought 1900 was a leap year (claiming to copy the error from
    earlier DOS spreadsheets), and Matlab's designer chose the
    non-existent date of \sQuote{January 0, 0000} (there is no such day),
    not specifying the calendar.  (There is such a year in the
    \sQuote{Gregorian} calendar as used in ISO 8601:2004, but that does say
    that it is only to be used for years before 1582 with the agreement
    of the parties in information exchange.)
  }
  The only safe procedure is to check the other systems values for known
  dates: reports on the Internet (including R-help) are more often wrong
  than right.
}
\references{
  International Organization for Standardization (2004, 1988, 1997,
  \dots)
  \emph{ISO 8601. Data elements and interchange formats --
    Information interchange -- Representation of dates and times.}
  For links to versions available on-line see (at the time of writing)
  \url{http://www.qsl.net/g1smd/isopdf.htm}; for information on the
  current official version, see
  \url{http://www.iso.org/iso/en/prods-services/popstds/datesandtime.html}.
}
\seealso{
  \link{Date} for details of the date class;
  \code{\link{locales}} to query or set a locale.

  Your system's help pages on \code{strftime} and \code{strptime} to see
  how to specify their formats.  Windows users will find no help page
  for \code{strptime}: code based on \samp{glibc} is used (with
  corrections), so all the format specifiers described here are
  supported, but with no alternative number representation nor era
  available in any locale.
}
\examples{\donttest{
## locale-specific version of the date
format(Sys.Date(), "\%a \%b \%d")}

## read in date info in format 'ddmmmyyyy'
## This will give NA(s) in some locales; setting the C locale
## as in the commented lines will overcome this on most systems.
## lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
x <- c("1jan1960", "2jan1960", "31mar1960", "30jul1960")
z <- as.Date(x, "\%d\%b\%Y")
## Sys.setlocale("LC_TIME", lct)
z

## read in date/time info in format 'm/d/y'
dates <- c("02/27/92", "02/27/92", "01/14/92", "02/28/92", "02/01/92")
as.Date(dates, "\%m/\%d/\%y")

## date given as number of days since 1900-01-01 (a date in 1989)
as.Date(32768, origin = "1900-01-01")
## Excel is said to use 1900-01-01 as day 1 (Windows default) or
## 1904-01-01 as day 0 (Mac default), but this is complicated by Excel
## treating 1900 as a leap year.
## So for dates (post-1901) from Windows Excel
as.Date(35981, origin = "1899-12-30") # 1998-07-05
## and Mac Excel
as.Date(34519, origin = "1904-01-01") # 1998-07-05
## (these values come from http://support.microsoft.com/kb/214330)

## Experiment shows that Matlab's origin is 719529 days before ours,
## so Matlab day 734373 can be imported as
as.Date(734373, origin = "1970-01-01") - 719529
## (value from http://www.mathworks.com/help/techdoc/matlab_prog/bspgcx2-1.html)

## Time zone effect
z <- ISOdate(2010, 04, 13, c(0,12)) # midnight and midday UTC
as.Date(z) # in UTC
\donttest{## these time zone names are common
as.Date(z, tz = "NZ")
as.Date(z, tz = "HST") # Hawaii}
}
\keyword{utilities}
\keyword{chron}
