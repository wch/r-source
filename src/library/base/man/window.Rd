\name{window}
\title{Time Windows}
\usage{
window(x, start, end, frequency, deltat)
}
\alias{window}
\alias{window.default}
\alias{window.ts}
\arguments{
\item{x}{a time-series or other object.}
\item{start}{the start time of the period of interest.}
\item{end}{the end time of the period of interest.}
\item{frequency}{the new sampling frequency, which should be a sub-multiple
    of the original frequency.}
\item{deltat}{the new time interval between observations, which should
    be a multiple of the original frequency.}
}
\description{
    \code{window} is a generic function which
    extracts the subset of the object \code{x}
    observed between the times \code{start} and \code{end}.
}
\details{
    The default method applies to objects with a \code{\link{tsp}}
    attribute, or vectors or matrices (which are assumed to have
    been observed at times \code{1} to \code{NROW(x)}).

    The start and end times are \emph{rounded} to the nearest sampling
    time. This is for S compatibility.
}
\value{
    An object with a \code{tsp} attribute, and with class that of the
    \code{x} unless (as for time series) that class is changed by
    subsetting. The \code{ts} ensures the result has class \code{"ts"}. 
}
\seealso{\code{\link{time}}, \code{\link{ts}}.}
\examples{
data(presidents)
sixties <- window(presidents, 1960, c(1969,4))

data(sunspots)
## all June values in 20th century
window(sunspots, start=c(1901,6), frequency=1)
}
\keyword{ts}
