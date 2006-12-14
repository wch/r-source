\name{date}
\alias{date}
\title{System Date and Time}
\description{
  Returns a character string of the current system date and time.
}
\usage{date()}
\value{
  The string has the form \code{"Fri Aug 20 11:11:00 1999"}, i.e.,
  length 24, since it relies on POSIX's \code{ctime} ensuring the above
  fixed format.  Timezone and Daylight Saving Time are taken account of,
  but \emph{not} indicated in the result.

  The day and month abbreviations are always in English, irrespective
  of locale.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{Sys.time}}; \code{\link{Date}} and
  \code{\link{DateTimeClasses}} for objects
  representing date and time.
}
\examples{
(d <- date())
nchar(d) == 24

## something similar in the current locale
format(Sys.time(), "\%a \%b \%d \%H:\%M:\%S \%Y")
}
\keyword{utilities}
