% File src/library/stats/man/window.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{window}
\alias{window}
\alias{window.default}
\alias{window.ts}
\alias{window<-}
\alias{window<-.ts}
\title{Time Windows}

\usage{
window(x, \dots)
\method{window}{ts}(x, \dots)
\method{window}{default}(x, start = NULL, end = NULL,
      frequency = NULL, deltat = NULL, extend = FALSE, \dots)

window(x, \dots) <- value
\method{window}{ts}(x, start, end, frequency, deltat, \dots) <- value
}
\arguments{
  \item{x}{a time-series (or other object if not replacing values).}
  \item{start}{the start time of the period of interest.}
  \item{end}{the end time of the period of interest.}
  \item{frequency, deltat}{the new frequency can be specified by either
    (or both if they are consistent).}
  \item{extend}{logical.  If true, the \code{start} and \code{end} values
    are allowed to extend the series.  If false, attempts to extend the
    series give a warning and are ignored.}
  \item{\dots}{further arguments passed to or from other methods.}
  \item{value}{replacement values.}
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

  The replacement function has a method for \code{ts} objects, and
  is allowed to extend the series (with a warning).  There is no default
  method.
}
\value{
  The value depends on the method. \code{window.default} will return a
  vector or matrix with an appropriate \code{\link{tsp}} attribute.

  \code{window.ts} differs from \code{window.default} only in
  ensuring the result is a \code{ts} object.

  If \code{extend = TRUE} the series will be padded with \code{NA}s if
  needed.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{\code{\link{time}}, \code{\link{ts}}.}
\examples{
window(presidents, 1960, c(1969,4)) # values in the 1960's
window(presidents, deltat = 1)  # All Qtr1s
window(presidents, start = c(1945,3), deltat = 1)  # All Qtr3s
window(presidents, 1944, c(1979,2), extend = TRUE)

pres <- window(presidents, 1945, c(1949,4)) # values in the 1940's
window(pres, 1945.25, 1945.50) <- c(60, 70)
window(pres, 1944, 1944.75) <- 0 # will generate a warning
window(pres, c(1945,4), c(1949,4), frequency = 1) <- 85:89
pres
}
\keyword{ts}
