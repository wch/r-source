\name{aggregate}
\alias{aggregate}
\alias{aggregate.default}
\alias{aggregate.data.frame}
\alias{aggregate.ts}
\title{Compute Summary Statistics of Data Subsets}
\usage{
aggregate(x, \dots)

\method{aggregate}{default}(x, \dots)

\method{aggregate}{data.frame}(x, by, FUN, \dots)

\method{aggregate}{ts}(x, nfrequency = 1, FUN = sum, ndeltat = 1,
          ts.eps = getOption("ts.eps"), \dots)
}
\description{
  Splits the data into subsets, computes summary statistics for each,
  and returns the result in a convenient form.
}
\arguments{
  \item{x}{an R object.}
  \item{by}{a list of grouping elements, each as long as the variables
    in \code{x}.  Names for the grouping variables are provided if
    they are not given.}
  \item{FUN}{a scalar function to compute the summary statistics which
    can be applied to all data subsets.}
  \item{nfrequency}{new number of observations per unit of time; must
    be a divisor of the frequency of \code{x}.}
  \item{ndeltat}{new fraction of the sampling period between
    successive observations; must be a divisor of the sampling
    interval of \code{x}.}
  \item{ts.eps}{tolerance used to decide if \code{nfrequency} is a
    sub-multiple of the original frequency.}
  \item{\dots}{further arguments passed to or used by methods.}
}
\details{
  \code{aggregate} is a generic function with methods for data frames
  and time series.
  
  The default method \code{aggregate.default} uses the time series
  method if \code{x} is a time series, and otherwise coerces \code{x}
  to a data frame and calls the data frame method.

  \code{aggregate.data.frame} is the data frame method.  If \code{x}
  is not a data frame, it is coerced to one.  Then, each of the
  variables (columns) in \code{x} is split into subsets of cases
  (rows) of identical combinations of the components of \code{by}, and
  \code{FUN} is applied to each such subset with further arguments in
  \code{\dots} passed to it.
  (I.e., \code{tapply(VAR, by, FUN, \dots, simplify = FALSE)} is done
  for each variable \code{VAR} in \code{x}, conveniently wrapped into
  one call to \code{lapply()}.)
  Empty subsets are removed, and the result is reformatted into a data
  frame containing the variables in \code{by} and \code{x}.  The ones
  arising from \code{by} contain the unique combinations of grouping
  values used for determining the subsets, and the ones arising from
  \code{x} the corresponding summary statistics for the subset of the
  respective variables in \code{x}.
  
  \code{aggregate.ts} is the time series method.  If \code{x} is not a
  time series, it is coerced to one.  Then, the variables in \code{x}
  are split into appropriate blocks of length
  \code{frequency(x) / nfrequency}, and \code{FUN} is applied to each
  such block, with further (named) arguments in \code{\dots} passed to
  it.  The result returned is a time series with frequency
  \code{nfrequency} holding the aggregated values.
}
\value{
  For the time series method, a time series of class \code{"ts"} or
  class \code{c("mts", "ts")}.

  For the data frame method, a data frame with columns
  corresponding to the grouping variables in \code{by} followed by
  aggregated columns from \code{x}.  \strong{Note:} prior to \R 2.6.0
  the grouping variables were reported as factors with levels in
  alphabetical order in the current locale.  Now the variable in the
  result is found by subsetting the original variable.
}
\author{Kurt Hornik}
\seealso{
  \code{\link{apply}}, \code{\link{lapply}}, \code{\link{tapply}}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\examples{
## Compute the averages for the variables in 'state.x77', grouped
## according to the region (Northeast, South, North Central, West) that
## each state belongs to.
aggregate(state.x77, list(Region = state.region), mean)

## Compute the averages according to region and the occurrence of more
## than 130 days of frost.
aggregate(state.x77,
          list(Region = state.region,
               Cold = state.x77[,"Frost"] > 130),
          mean)
## (Note that no state in 'South' is THAT cold.)

## Compute the average annual approval ratings for American presidents.
aggregate(presidents, nfrequency = 1, FUN = mean)
## Give the summer less weight.
aggregate(presidents, nfrequency = 1,
          FUN = weighted.mean, w = c(1, 1, 0.5, 1))
}
\keyword{category}
\keyword{array}
