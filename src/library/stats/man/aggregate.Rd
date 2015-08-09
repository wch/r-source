% File src/library/stats/man/aggregate.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{aggregate}
\alias{aggregate}
\alias{aggregate.default}
\alias{aggregate.data.frame}
\alias{aggregate.formula}
\alias{aggregate.ts}
\title{Compute Summary Statistics of Data Subsets}
\usage{
aggregate(x, \dots)

\method{aggregate}{default}(x, \dots)

\method{aggregate}{data.frame}(x, by, FUN, \dots, simplify = TRUE, drop = TRUE)

\method{aggregate}{formula}(formula, data, FUN, \dots,
          subset, na.action = na.omit)

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
    in the data frame \code{x}.  The elements are coerced to factors
    before use.}
  \item{FUN}{a function to compute the summary statistics which can be
    applied to all data subsets.}
  \item{simplify}{a logical indicating whether results should be
    simplified to a vector or matrix if possible.}
  \item{drop}{a logical indicating whether to drop unused combinations
    of grouping values.}
  \item{formula}{a \link{formula}, such as \code{y ~ x} or
    \code{cbind(y1, y2) ~ x1 + x2}, where the \code{y} variables are
    numeric data to be split into groups according to the grouping
    \code{x} variables (usually factors).}
  \item{data}{a data frame (or list) from which the variables in formula
    should be taken.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used.}
  \item{na.action}{a function which indicates what should happen when
    the data contain \code{NA} values. The default is to ignore missing
    values in the given variables.}
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

  The default method, \code{aggregate.default}, uses the time series
  method if \code{x} is a time series, and otherwise coerces \code{x}
  to a data frame and calls the data frame method.

  \code{aggregate.data.frame} is the data frame method.  If \code{x} is
  not a data frame, it is coerced to one, which must have a non-zero
  number of rows.  Then, each of the variables (columns) in \code{x} is
  split into subsets of cases (rows) of identical combinations of the
  components of \code{by}, and \code{FUN} is applied to each such subset
  with further arguments in \code{\dots} passed to it.  The result is
  reformatted into a data frame containing the variables in \code{by}
  and \code{x}.  The ones arising from \code{by} contain the unique
  combinations of grouping values used for determining the subsets, and
  the ones arising from \code{x} the corresponding summaries for the
  subset of the respective variables in \code{x}.  If \code{simplify} is
  true, summaries are simplified to vectors or matrices if they have a
  common length of one or greater than one, respectively; otherwise,
  lists of summary results according to subsets are obtained.  Rows with
  missing values in any of the \code{by} variables will be omitted from
  the result.  (Note that versions of \R prior to 2.11.0 required
  \code{FUN} to be a scalar function.)

  \code{aggregate.formula} is a standard formula interface to
  \code{aggregate.data.frame}.

  \code{aggregate.ts} is the time series method, and requires \code{FUN}
  to be a scalar function.  If \code{x} is not a time series, it is
  coerced to one.  Then, the variables in \code{x} are split into
  appropriate blocks of length \code{frequency(x) / nfrequency}, and
  \code{FUN} is applied to each such block, with further (named)
  arguments in \code{\dots} passed to it.  The result returned is a time
  series with frequency \code{nfrequency} holding the aggregated values.
  Note that this make most sense for a quarterly or yearly result when
  the original series covers a whole number of quarters or years: in
  particular aggregating a monthly series to quarters starting in
  February does not give a conventional quarterly series.

  \code{FUN} is passed to \code{\link{match.fun}}, and hence it can be a
  function or a symbol or character string naming a function.
}
\value{
  For the time series method, a time series of class \code{"ts"} or
  class \code{c("mts", "ts")}.

  For the data frame method, a data frame with columns
  corresponding to the grouping variables in \code{by} followed by
  aggregated columns from \code{x}.  If the \code{by} has names, the
  non-empty times are used to label the columns in the results, with
  unnamed grouping variables being named \code{Group.\var{i}} for
  \code{by[[\var{i}]]}.
}
\author{
  Kurt Hornik, with contributions by Arni Magnusson.
}
\seealso{
  \code{\link{apply}}, \code{\link{lapply}}, \code{\link{tapply}}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
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


## example with character variables and NAs
testDF <- data.frame(v1 = c(1,3,5,7,8,3,5,NA,4,5,7,9),
                     v2 = c(11,33,55,77,88,33,55,NA,44,55,77,99) )
by1 <- c("red", "blue", 1, 2, NA, "big", 1, 2, "red", 1, NA, 12)
by2 <- c("wet", "dry", 99, 95, NA, "damp", 95, 99, "red", 99, NA, NA)
aggregate(x = testDF, by = list(by1, by2), FUN = "mean")

# and if you want to treat NAs as a group
fby1 <- factor(by1, exclude = "")
fby2 <- factor(by2, exclude = "")
aggregate(x = testDF, by = list(fby1, fby2), FUN = "mean")


## Formulas, one ~ one, one ~ many, many ~ one, and many ~ many:
aggregate(weight ~ feed, data = chickwts, mean)
aggregate(breaks ~ wool + tension, data = warpbreaks, mean)
aggregate(cbind(Ozone, Temp) ~ Month, data = airquality, mean)
aggregate(cbind(ncases, ncontrols) ~ alcgp + tobgp, data = esoph, sum)

## Dot notation:
aggregate(. ~ Species, data = iris, mean)
aggregate(len ~ ., data = ToothGrowth, mean)

## Often followed by xtabs():
ag <- aggregate(len ~ ., data = ToothGrowth, mean)
xtabs(len ~ ., data = ag)


## Compute the average annual approval ratings for American presidents.
aggregate(presidents, nfrequency = 1, FUN = mean)
## Give the summer less weight.
aggregate(presidents, nfrequency = 1,
          FUN = weighted.mean, w = c(1, 1, 0.5, 1))
}
\keyword{category}
\keyword{array}
