% file modreg/man/supsmu.Rd
% copyright (C) 1998 B. D. Ripley
%
\name{supsmu}
\title{Friedman's SuperSmoother}
\usage{
supsmu(x, y, wt=rep(1, length(y)), span="cv", periodic=FALSE, bass=0)
}
\alias{supsmu}
\arguments{
 \item{x}{x values for smoothing}
 \item{y}{y values for smoothing}
 \item{wt}{case weights}
 \item{span}{the fraction of the observations in the span of the running
   lines smoother, or \code{"cv"} to choose this by leave-one-out
   cross-validation.}
 \item{periodic}{If true, the x values are assumed to be in \code{[0,
     1]} and of period 1.}
 \item{bass}{controls the smoothness of the fitted curve. Values of up
   to 10 indicating increasing smoothness.}
}
\description{
 Smooth the (x, y) values by Friedman's "super smoother".
}
\details{
 \code{supsmu} is a running lines smoother which chooses between three
 spans for the lines. The running lines smoothers are symmetric, with
 \code{k/2} data points each side of the predicted point, and values of
 \code{k} as 0.5n, 0.2n and 0.05n. If \code{span} is specified, as
 single smoother with span \code{span} * n is used.

 The best of the three smoothers is chosen by cross-validation for each
 prediction. The best spans are then smoothed by a running lines
 smoother and the final prediction chosen by linear interpolation. 
 
 The Fortran code says: "For small samples (n < 40) or if there are
 substantial serial correlations between observations close in x -
 value, then a prespecified fixed span smoother (span > 0) should be
 used. Reasonable span values are 0.2 to 0.4."
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
\author{B.D. Ripley}
\seealso{\code{\link{ppr}}}

\examples{
data(cars)
attach(cars)
plot(speed, dist)
lines(supsmu(speed, dist))
lines(supsmu(speed, dist, bass=7), lty=2)
detach()
}
\keyword{smooth}
