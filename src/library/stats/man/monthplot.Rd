\name{monthplot}
\alias{monthplot}
\alias{monthplot.default}
\alias{monthplot.ts}
\alias{monthplot.stl}
\alias{monthplot.StructTS}

\title{
  Plot a Seasonal or other Subseries
}
\description{
  These functions plot seasonal (or other) subseries of a time series.
  For each season (or other category), a time series is plotted.
}
\synopsis{
monthplot(x, \dots)
monthplot.StructTS(x, labels = NULL, ylab = choice, choice = "sea", \dots)
monthplot.stl(x, labels = NULL, ylab = choice, choice = "seasonal", \dots)
monthplot.ts(x, labels = NULL, times = time(x), phase = cycle(x),
             ylab = deparse(substitute(x)), \dots)
monthplot.default(x, labels = 1:12,
              times = 1:length(x),
              phase = (times - 1)\%\%length(labels) + 1,
              base = mean,
              xlim = c(0.55, f + 0.45), ylim = range(x, na.rm = TRUE),
              axes = TRUE, xlab = "", ylab = deparse(substitute(x)),
              type = "l", box = TRUE, add = FALSE, \dots)
}
\usage{
monthplot(x, labels = NULL, times, phase, base, choice, \ldots)
}
\arguments{
  \item{x}{Time series or related object.}
  \item{labels}{Labels to use for each \dQuote{season}.}
  \item{times}{Time of each observation.}
  \item{phase}{Indicator for each \dQuote{season}.}
  \item{base}{Function to use for reference line for subseries.}
  \item{choice}{Which series of an \code{stl} or \code{StructTS} object?}
  \item{\dots}{Graphical parameters.}
}
\details{
  These functions extract subseries from a time series and plot them
  all in one frame.  The \code{\link{ts}}, \code{\link{stl}}, and
  \code{\link{StructTS}} methods use the internally recorded frequency and
  start and finish times to set the scale and the seasons.  The default
  method assumes observations come in groups of 12 (though this can be
  changed).

  If the \code{labels} are not given but the \code{phase} is given, then
  the \code{labels} default to the unique values of the \code{phase}.  If
  both are given, then the \code{phase} values are assumed to be indices
  into the \code{labels} array, i.e., they should be in the range
  from 1 to \code{length(labels)}.
}
\value{
  These functions are executed for their side effect of
  drawing a seasonal subseries plot on the current graphical
  window.
}
\author{Duncan Murdoch}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{ts}}, \code{\link{stl}}, \code{\link{StructTS}}
}

\examples{
## The CO2 data
data(co2)
fit <- stl(log(co2), s.window = 20, t.window = 20)
plot(fit)
op <- par(mfrow = c(2,2))
monthplot(co2, ylab = "data", cex.axis = 0.8)
monthplot(fit, choice = "seasonal", cex.axis = 0.8)
monthplot(fit, choice = "trend", cex.axis = 0.8)
monthplot(fit, choice = "remainder", type = "h", cex.axis = 0.8)
par(op)

## The CO2 data, grouped quarterly
quarter <- (cycle(co2) - 1) \%/\% 3
monthplot(co2, phase = quarter)

## see also JohnsonJohnson
}
\keyword{ hplot }
\keyword{ ts }
