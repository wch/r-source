\name{window}
\title{Time Windows}
\usage{
window(x, ...)
window.default(x, start = NULL, end = NULL,
               frequency = NULL, deltat = NULL, extend = FALSE)
window.ts(x, start = NULL, end = NULL,
          frequency = NULL, deltat = NULL, extend = FALSE)
}
\alias{window}
\alias{window.default}
\alias{window.ts}
\arguments{
  \item{x}{a time-series or other object.}
  \item{start}{the start time of the period of interest.}
  \item{end}{the end time of the period of interest.}
  \item{frequency, deltat}{the new frequency can be specified by either
    (or both if they are consistent).}
  \item{extend}{logical. If true, the \code{start} and \code{end} values
    are allowed to extend the series. If false, attempts to extend the
    series give a warning and are ignored.}
}
\description{
  \code{window} is a generic function which
  extracts the subset of the object \code{x}
  observed between the times \code{start} and \code{end}. If a
  frequency is specified, the series is then re-sampled at the new
  frequency.
}
\details{
  The start and end times can be specified as for \code{\link{ts}}. If
  there is no observation at the new \code{start} or \code{end},
  the immediately following (\code{start}) or preceding (\code{end})
  observation time is used.
}
\value{
  The value depends on the method. \code{window.default} will return a
  vector or matrix with an appropriate \code{\link{tsp}} attribute.

  \code{window.ts} differs from \code{window.default} only in
  ensuring the result is a \code{ts} object.

  If \code{extend = TRUE} the series will be padded with \code{NA} if
  needed.
}
\seealso{\code{\link{time}}, \code{\link{ts}}.}
\examples{
data(presidents)
window(presidents, 1960, c(1969,4)) # values in the 1960's
window(presidents, deltat=1)  # All Qtr1s
window(presidents, start=c(1945,3), deltat=1)  # All Qtr3s
window(presidents, 1944, c(1979,2), extend=TRUE)
}
\keyword{ts}
