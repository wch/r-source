\name{ts}
\title{Time-Series Objects}
\usage{
ts(data = NA, start = 1, end = numeric(0), frequency = 1,
   deltat = 1, ts.eps = .Options$ts.eps)
as.ts(x)
is.ts(x)

print(ts.obj, calendar, ...)
plot(ts.obj, ...)
}
\alias{ts}
\alias{as.ts}
\alias{is.ts}
\alias{print.ts}
\alias{plot.ts}
\arguments{
  \item{data}{a vector or matrix the observed time-series values.}
  \item{start}{the time of the first observation.}
  \item{end}{the time of the last observation.}
  \item{frequency}{the number of observations per unit of time.}
  \item{deltat}{the fraction of the sampling period between successive
    observations; e.g., 1/12 for monthly data.
    Only one of \code{frequency} or \code{deltat} should be provided.}
  \item{ts.eps}{time series comparison tolerance.  Frequencies are
    considered equal if their absolute difference is less than
    \code{ts.eps}.}
}
\description{
  The function \code{ts} is used to create time-series objects.  These
  are vector or matrices with class of \code{"ts"} (and additional
  attributes) which represent data which has been sampled at equispaced
  points in time.  In the matrix case, each column of the matrix
  \code{data} is assumed to contain a single (univariate) time series.

  The value of argument \code{frequency} is used when the series is
  sampled an interal number of times in each unit time interval.  For
  example, one could use a value of \code{7} for \code{frequency} when
  the data are sampled daily, and the natural time period is a week, or
  \code{12} when the data are sampled monthly and the natural time
  period is a year.

  \code{start} and \code{end} can either be integers which correspond to
  natural time units, or vectors of two integers, which give a natural
  time unit and a (1-based) number of samples into the time unit.

  \code{as.ts} and \code{is.ts} respectively coerce a vector into a
  time-series and test whether an object is a time series.

  Time series have methods associated with the generic \code{print} and
  \code{plot} functions.  The argument \code{calendar} to the print
  method can be used to enable/disable the display of information about
  month names, quarter names or year when printing.
}
\seealso{
  \code{\link{frequency}},
  \code{\link{start}},
  \code{\link{end}},
  \code{\link{time}},
  \code{\link{window}}.
}
\examples{
ts(1:10, frequency = 4, start = c(1959, 2)) # 2nd Quarter of 1959
print( ts(1:10, freq = 7, start = c(12, 2)), calendar = TRUE) # print.ts(.)
## Using July 1954 as start date:
gnp <- ts(cumsum(1 + round(rnorm(100), 2)),
          start = c(1954, 7), frequency = 12)
plot(gnp) # using `plot.ts' for time-series plot
}
\keyword{ts}
