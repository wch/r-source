\name{date}
\alias{date}
\title{System Date and Time}
\usage{date()}
\description{Returns a character string of the current system date and
    time.}
\value{
    The string has the form \code{"Fri Aug 20 11:11:00 1999"},
    i.e. length 24, since it relies on POSIX' \code{ctime}
    ensuring the above fixed format.  Timezone and Daylight Saving Time are
    taken account of, but \emph{not} indicated in the result.
}
\examples{
(d <- date())
nchar(d) == 24
}
\keyword{utilities}
