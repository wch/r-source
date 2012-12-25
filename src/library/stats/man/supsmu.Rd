% File src/library/stats/man/supsmu.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{supsmu}
\alias{supsmu}
\title{Friedman's SuperSmoother}
\description{
  Smooth the (x, y) values by Friedman's \sQuote{super smoother}.
}
\usage{
supsmu(x, y, wt, span = "cv", periodic = FALSE, bass = 0)
}
\arguments{
  \item{x}{x values for smoothing}
  \item{y}{y values for smoothing}
  \item{wt}{case weights, by default all equal}
  \item{span}{the fraction of the observations in the span of the running
    lines smoother, or \code{"cv"} to choose this by leave-one-out
    cross-validation.}
  \item{periodic}{if \code{TRUE}, the x values are assumed to be in
    \code{[0, 1]} and of period 1.}
  \item{bass}{controls the smoothness of the fitted curve. Values of up
    to 10 indicate increasing smoothness.}
}
\details{
  \code{supsmu} is a running lines smoother which chooses between three
  spans for the lines. The running lines smoothers are symmetric, with
  \code{k/2} data points each side of the predicted point, and values of
  \code{k} as \code{0.5 * n}, \code{0.2 * n} and \code{0.05 * n}, where
  \code{n} is the number of data points.  If \code{span} is specified,
  a single smoother with span \code{span * n} is used.

  The best of the three smoothers is chosen by cross-validation for each
  prediction. The best spans are then smoothed by a running lines
  smoother and the final prediction chosen by linear interpolation.

  The FORTRAN code says: \dQuote{For small samples (\code{n < 40}) or if
    there are substantial serial correlations between observations close
    in x-value, then a pre-specified fixed span smoother (\code{span >
      0}) should be used.  Reasonable span values are 0.2 to 0.4.}

  Cases with non-finite values of \code{x}, \code{y} or \code{wt} are
  dropped, with a warning.
}
\value{
  A list with components
  \item{x}{the input values in increasing order with duplicates removed.}
  \item{y}{the corresponding y values on the fitted curve.}
}
\references{
  Friedman, J. H. (1984)
  SMART User's Guide.
  Laboratory for Computational Statistics, Stanford University Technical
  Report No. 1.

  Friedman, J. H. (1984)
  A variable span scatterplot smoother.
  Laboratory for Computational Statistics, Stanford University Technical
  Report No. 5.
}
\seealso{\code{\link{ppr}}}

\examples{
require(graphics)

with(cars, {
    plot(speed, dist)
    lines(supsmu(speed, dist))
    lines(supsmu(speed, dist, bass = 7), lty = 2)
    })
}
\keyword{smooth}
