\name{format.Date}
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

\title{Date Conversion Functions to and from Character}
\description{
  Functions to convert between character representations and objects of
  class \code{"Date"} representing calendar dates.
}
\usage{
as.Date(x, \ldots)
\method{as.Date}{character}(x, format = "", \dots)

\method{format}{Date}(x, \dots)

\method{as.character}{Date}(x, \dots)
}
\arguments{
  \item{x}{An object to be converted.}
  \item{format}{A character string.  The default is
    \code{"\%Y-\%m-\%d"}.  For details see \code{\link{strftime}}.}
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
  \code{"\link{POSIXct}"}.  (The last are converted to days by
  ignoring the time after midnight in the
  representation of the time in UTC.)  Also objects of class
  \code{"date"} (from package \pkg{\link[date:as.date]{date}} or
  \pkg{\link[survival:as.date]{survival}}) and
  \code{"dates"} (from package \pkg{\link[chron]{chron}}).  Character
  strings are processed as far as necessary for the format specified: any
  trailing characters are ignored.

  See the examples for how to convert a day given as the number of days
  since an epoch.

  The \code{format} and \code{as.character} methods ignore any
  fractional part of the date.
}
\value{
  The \code{format} and \code{as.character} methods return a character vector
  representing the date.

  The \code{as.Date} methods return an object of class \code{"\link{Date}"}.
}
\note{
  The default formats follow the rules of the ISO 8601 international
  standard which expresses a day as \code{"2001-02-03"}.

  If the date string does not specify the date completely, the returned
  answer may be system-specific.  The most common behaviour is to assume
  that a missing
  year, month or day is the current one.  If it specifies a date
  incorrectly, reliable implementations will give an error and the date
  is reported as \code{NA}.  Unfortunately some common implementations
  (such as \samp{glibc}) are unreliable and guess at the intended meaning.

  Years before 1CE (aka 1AD) will probably not be handled correctly.
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
  
  Your system's help pages on \code{strftime} and \code{strptime} to
  see how to specify their formats.
#ifdef windows
  Windows users will find no help page for \code{strptime}: code based
  on \samp{glibc} is used (with corrections), so all the format
  specifiers described here are supported, but with no alternative
  number representation nor era available in any locale.
#endif
}
\examples{
## locale-specific version of the date
format(Sys.Date(), "\%a \%b \%d")

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
as.Date("1900-01-01") + 32768
}
\keyword{utilities}
\keyword{chron}
