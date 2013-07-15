% File src/library/stats/man/ts.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{ts}
\title{Time-Series Objects}
\alias{ts}
\alias{as.ts}
\alias{as.ts.default}
\alias{is.ts}
\alias{Ops.ts}
\alias{cbind.ts}
\alias{is.mts}
\alias{[.ts}
\alias{t.ts}
\description{
  The function \code{ts} is used to create time-series objects.

  \code{as.ts} and \code{is.ts} coerce an object to a time-series and
  test whether an object is a time series.
}
\usage{
ts(data = NA, start = 1, end = numeric(), frequency = 1,
   deltat = 1, ts.eps = getOption("ts.eps"), class = , names = )
as.ts(x, \dots)
is.ts(x)
}
\arguments{
  \item{data}{a numeric vector or matrix of the observed time-series
    values. A data frame will be coerced to a numeric matrix via
    \code{data.matrix}.  (See also \sQuote{Details}.)}
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
    \code{c("mts", "ts", "matrix")} for multiple series.}
  \item{names}{a character vector of names for the series in a multiple
    series: defaults to the colnames of \code{data}, or \code{Series 1},
    \code{Series 2}, \dots.}
  \item{x}{an arbitrary \R object.}
  \item{\dots}{arguments passed to methods (unused for the default method).}
}
\details{
  The function \code{ts} is used to create time-series objects.  These
  are vector or matrices with class of \code{"ts"} (and additional
  attributes) which represent data which has been sampled at equispaced
  points in time.  In the matrix case, each column of the matrix
  \code{data} is assumed to contain a single (univariate) time series.
  Time series must have at least one observation, and although they need
  not be numeric there is very limited support for non-numeric series.

  Class \code{"ts"} has a number of methods.  In particular arithmetic
  will attempt to align time axes, and subsetting to extract subsets of
  series can be used (e.g., \code{EuStockMarkets[, "DAX"]}).  However,
  subsetting the first (or only) dimension will return a matrix or
  vector, as will matrix subsetting.  Subassignment can be used to
  replace values but not to extend a series (see \code{\link{window}}).
  There is a method for \code{\link{t}} that transposes the series as a
  matrix (a one-column matrix if a vector) and hence returns a result
  that does not inherit from class \code{"ts"}.

  The value of argument \code{frequency} is used when the series is
  sampled an integral number of times in each unit time interval.  For
  example, one could use a value of \code{7} for \code{frequency} when
  the data are sampled daily, and the natural time period is a week, or
  \code{12} when the data are sampled monthly and the natural time
  period is a year.  Values of \code{4} and \code{12} are assumed in
  (e.g.) \code{print} methods to imply a quarterly and monthly series
  respectively.

  \code{as.ts} is generic.  Its default method will use the
  \code{\link{tsp}} attribute of the object if it has one to set the
  start and end times and frequency.

  \code{is.ts} tests if an object is a time series.  It is generic: you
  can write methods to handle specific classes of objects,
  see \link{InternalMethods}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
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
}
\examples{
require(graphics)

ts(1:10, frequency = 4, start = c(1959, 2)) # 2nd Quarter of 1959
print( ts(1:10, frequency = 7, start = c(12, 2)), calendar = TRUE)
# print.ts(.)
## Using July 1954 as start date:
gnp <- ts(cumsum(1 + round(rnorm(100), 2)),
          start = c(1954, 7), frequency = 12)
plot(gnp) # using 'plot.ts' for time-series plot

## Multivariate
z <- ts(matrix(rnorm(300), 100, 3), start = c(1961, 1), frequency = 12)
class(z)
head(z) # as "matrix"
plot(z)
plot(z, plot.type = "single", lty = 1:3)

## A phase plot:
plot(nhtemp, c(nhtemp[-1], NA), cex = .8, col = "blue",
     main = "Lag plot of New Haven temperatures")
## a clearer way to do this would be
\dontrun{
plot(nhtemp, lag(nhtemp, 1), cex = .8, col = "blue",
     main = "Lag plot of New Haven temperatures")
}}
\keyword{ts}
