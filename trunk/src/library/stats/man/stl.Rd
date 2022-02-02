% File src/library/stats/man/stl.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{stl}
\alias{stl}
% all methods -> ./stlmethods.Rd
\title{Seasonal Decomposition of Time Series by Loess}
\description{
  Decompose a time series into seasonal, trend and irregular components
  using \code{loess}, acronym STL.
}
\usage{
stl(x, s.window, s.degree = 0,
    t.window = NULL, t.degree = 1,
    l.window = nextodd(period), l.degree = t.degree,
    s.jump = ceiling(s.window/10),
    t.jump = ceiling(t.window/10),
    l.jump = ceiling(l.window/10),
    robust = FALSE,
    inner = if(robust)  1 else 2,
    outer = if(robust) 15 else 0,
    na.action = na.fail)
}
\arguments{
  \item{x}{univariate time series to be decomposed.
    This should be an object of class \code{"ts"} with a frequency
    greater than one.}
  \item{s.window}{either the character string \code{"periodic"} or the span (in
    lags) of the loess window for seasonal extraction, which should
    be odd and at least 7, according to Cleveland et al.  This has no default.}
  \item{s.degree}{degree of locally-fitted polynomial in seasonal
    extraction.  Should be zero or one.}
  \item{t.window}{the span (in lags) of the loess window for trend
    extraction, which should be odd.  If \code{NULL}, the default,
    \code{nextodd(ceiling((1.5*period) / (1-(1.5/s.window))))}, is taken.}
  \item{t.degree}{degree of locally-fitted polynomial in trend
    extraction.  Should be zero or one.}
  \item{l.window}{the span (in lags) of the loess window of the low-pass
    filter used for each subseries.  Defaults to the smallest odd
    integer greater than or equal to \code{frequency(x)} which is
    recommended since it prevents competition between the trend and
    seasonal components.  If not an odd integer its given value is
    increased to the next odd one.}
  \item{l.degree}{degree of locally-fitted polynomial for the subseries
    low-pass filter.  Must be 0 or 1.}
  \item{s.jump, t.jump, l.jump}{integers at least one to increase speed of
    the respective smoother.  Linear interpolation happens between every
    \code{*.jump}th value.}
  \item{robust}{logical indicating if robust fitting be used in the
    \code{loess} procedure.}
  \item{inner}{integer; the number of \sQuote{inner} (backfitting)
    iterations; usually very few (2) iterations suffice.}
  \item{outer}{integer; the number of \sQuote{outer} robustness
    iterations.}
  \item{na.action}{action on missing values.}
}
\details{
  The seasonal component is found by \emph{loess} smoothing the
  seasonal sub-series (the series of all January values, \ldots); if
  \code{s.window = "periodic"} smoothing is effectively replaced by
  taking the mean. The seasonal values are removed, and the remainder
  smoothed to find the trend. The overall level is removed from the
  seasonal component and added to the trend component. This process is
  iterated a few times.  The \code{remainder} component is the
  residuals from the seasonal plus trend fit.

  Several methods for the resulting class \code{"stl"} objects, see,
  \code{\link{plot.stl}}.
}
\value{
  \code{stl} returns an object of class \code{"stl"} with components
  \item{time.series}{a multiple time series with columns
    \code{seasonal}, \code{trend} and \code{remainder}.}
  \item{weights}{the final robust weights (all one if fitting is not
    done robustly).}
  \item{call}{the matched call.}
  \item{win}{integer (length 3 vector) with the spans used for the \code{"s"},
    \code{"t"}, and \code{"l"} smoothers.}
  \item{deg}{integer (length 3) vector with the polynomial degrees for
    these smoothers.}
  \item{jump}{integer (length 3) vector with the \sQuote{jumps} (skips)
    used for these smoothers.}
  \item{ni}{number of \bold{i}nner iterations}
  \item{no}{number of \bold{o}uter robustness iterations}
}
\references{
  R. B. Cleveland, W. S. Cleveland, J.E.  McRae, and I. Terpenning (1990)
  STL:  A  Seasonal-Trend  Decomposition  Procedure Based on Loess.
  \emph{Journal of Official Statistics}, \bold{6}, 3--73.
}
\author{B.D. Ripley; Fortran code by Cleveland \emph{et al} (1990) from
  \file{netlib}.}

\note{This is similar to but not identical to the \code{stl} function in
  S-PLUS. The \code{remainder} component given by S-PLUS is the sum of
  the \code{trend} and \code{remainder} series from this function.}

\seealso{
  \code{\link{plot.stl}} for \code{stl} methods;
  \code{\link{loess}} in package \pkg{stats} (which is not actually
  used in \code{stl}).

  \code{\link{StructTS}} for different kind of decomposition.
}
\examples{
require(graphics)

plot(stl(nottem, "per"))
plot(stl(nottem, s.window = 7, t.window = 50, t.jump = 1))

plot(stllc <- stl(log(co2), s.window = 21))
summary(stllc)
## linear trend, strict period.
plot(stl(log(co2), s.window = "per", t.window = 1000))

## Two STL plotted side by side :
        stmd <- stl(mdeaths, s.window = "per") # non-robust
summary(stmR <- stl(mdeaths, s.window = "per", robust = TRUE))
op <- par(mar = c(0, 4, 0, 3), oma = c(5, 0, 4, 0), mfcol = c(4, 2))
plot(stmd, set.pars = NULL, labels  =  NULL,
     main = "stl(mdeaths, s.w = \"per\",  robust = FALSE / TRUE )")
plot(stmR, set.pars = NULL)
# mark the 'outliers' :
(iO <- which(stmR $ weights  < 1e-8)) # 10 were considered outliers
sts <- stmR$time.series
points(time(sts)[iO], 0.8* sts[,"remainder"][iO], pch = 4, col = "red")
par(op)   # reset
}
\keyword{ts}
