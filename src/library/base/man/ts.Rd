\name{ts}
\title{Time-Series Objects}
\alias{ts}
\alias{as.ts}
\alias{is.ts}
\description{
  The function \code{ts} is used to create time-series objects.

  \code{as.ts} and \code{is.ts} coerce an object to a time-series and
  test whether an object is a time series.
}
\usage{
ts(data = NA, start = 1, end = numeric(0), frequency = 1,
   deltat = 1, ts.eps = getOption("ts.eps"), class, names)
as.ts(x)
is.ts(x)
}
\arguments{
  \item{data}{a vector or matrix of the observed time-series values.}
  \item{start}{the time of the first observation.  Either a single
    number or a vector of two integers, which specify a natural time
    unit and a (1-based) number of samples into the time unit.  See 
    the examples for the use of the second form.}
  \item{end}{the time of the last observation, specified in the same way
    as \code{start}.}
  \item{frequency}{the number of observations per unit of time.}
  \item{deltat}{the fraction of the sampling period between successive
    observations; e.g., 1/12 for monthly data.  Only one of
    \code{frequency} or \code{deltat} should be provided.}
  \item{ts.eps}{time series comparison tolerance.  Frequencies are
    considered equal if their absolute difference is less than
    \code{ts.eps}.}
  \item{class}{class to be given to the result, or none if \code{NULL}
    or \code{"none"}.  The default is \code{"ts"} for a single series,
    \code{c("mts", "ts")} for multiple series.}
  \item{names}{a character vector of names for the series in a multiple
    series: defaults to the colnames of \code{data}, or \code{Series 1},
    \code{Series 2}, \dots.}
  \item{x}{an arbitrary \R object.}
}
\details{
  The function \code{ts} is used to create time-series objects.  These
  are vector or matrices with class of \code{"ts"} (and additional
  attributes) which represent data which has been sampled at equispaced
  points in time.  In the matrix case, each column of the matrix
  \code{data} is assumed to contain a single (univariate) time series.

  Class \code{"ts"} has a number of methods.  In particular arithmetic
  will attempt to align time axes, and subsetting to extract subsets of
  series can be used (e.g. \code{EuStockMarkets[, "DAX"]}).  However,
  subsetting the first (or only) dimension will return a matrix or
  vector, as will matrix subsetting.

  The value of argument \code{frequency} is used when the series is
  sampled an integral number of times in each unit time interval.  For
  example, one could use a value of \code{7} for \code{frequency} when
  the data are sampled daily, and the natural time period is a week, or
  \code{12} when the data are sampled monthly and the natural time
  period is a year.  Values of \code{4} and \code{12} are assumed in
  (e.g.) \code{print} methods to imply a quarterly and monthly series
  respectively.

  \code{as.ts} will use the \code{\link{tsp}} attribute of the object if
  it has one to set the start and end times and frequency.
}
\seealso{
  \code{\link{tsp}},
  \code{\link{frequency}},
  \code{\link{start}},
  \code{\link{end}},
  \code{\link{time}},
  \code{\link{window}};
  \code{\link{print.ts}}, the print method for time series objects;
  \code{\link{plot.ts}}, the plot method for time series objects.
  Standard package \bold{ts} for many additional time-series functions.
}
\examples{
ts(1:10, frequency = 4, start = c(1959, 2)) # 2nd Quarter of 1959
print( ts(1:10, freq = 7, start = c(12, 2)), calendar = TRUE) # print.ts(.)
## Using July 1954 as start date:
gnp <- ts(cumsum(1 + round(rnorm(100), 2)),
          start = c(1954, 7), frequency = 12)
plot(gnp) # using `plot.ts' for time-series plot

## Multivariate
z <- ts(matrix(rnorm(300), 100, 3), start=c(1961, 1), frequency=12)
class(z)
plot(z)
plot(z, plot.type="single", lty=1:3)

# Ensure working arithmetic for `ts' objects :
stopifnot(z == z)
stopifnot(z-z == 0)

\testonly{
 ts(1:5, start=2, end=4) # truncate
 ts(1:5, start=3, end=17)# repeat
}
## A phase plot:
data(nhtemp)
plot(nhtemp, c(nhtemp[-1], NA), cex = .8, col="blue",
     main = "Lag plot of New Haven temperatures")
## a clearer way to do this would be
\dontrun{library(ts)
plot(nhtemp, lag(nhtemp, 1), cex = .8, col="blue",
     main = "Lag plot of New Haven temperatures")
}}
\keyword{ts}
