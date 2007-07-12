\name{loess}
\alias{loess}
\title{Local Polynomial Regression Fitting}
\usage{
loess(formula, data, weights, subset, na.action, model = FALSE,
      span = 0.75, enp.target, degree = 2,
      parametric = FALSE, drop.square = FALSE, normalize = TRUE,
      family = c("gaussian", "symmetric"),
      method = c("loess", "model.frame"),
      control = loess.control(\dots), \dots)
}
\arguments{
  \item{formula}{a \link{formula} specifying the numeric response and
    one to four numeric predictors (best specified via an interaction,
    but can also be specified additively). Will be coerced to a formula
    if necessary.}
  \item{data}{an optional data frame, list or environment (or object
    coercible by \code{\link{as.data.frame}} to a data frame) containing
    the variables in the model.  If not found in \code{data}, the
    variables are taken from \code{environment(formula)},
    typically the environment from which \code{loess} is called.}
  \item{weights}{optional weights for each case.}
  \item{subset}{an optional specification of a subset of the data to be
    used.}
  \item{na.action}{the action to be taken with missing values in the
    response or predictors.  The default is given by
    \code{getOption("na.action")}.}
  \item{model}{should the model frame be returned?}
  \item{span}{the parameter \eqn{\alpha} which controls the degree of
    smoothing.}
  \item{enp.target}{an alternative way to specify \code{span}, as the
    approximate equivalent number of parameters to be used.}
  \item{degree}{the degree of the polynomials to be used, up to 2.}
  \item{parametric}{should any terms be fitted globally rather than
    locally?  Terms can be specified by name, number or as a logical
    vector of the same length as the number of predictors.}
  \item{drop.square}{for fits with more than one predictor and
    \code{degree=2}, should the quadratic term (and cross-terms) be
    dropped for particular predictors?  Terms are specified in the same
    way as for \code{parametric}.}
  \item{normalize}{should the predictors be normalized to a common scale
    if there is more than one?  The normalization used is to set the
    10\% trimmed standard deviation to one.  Set to false for spatial
    coordinate predictors and others know to be a common scale.}
  \item{family}{if \code{"gaussian"} fitting is by least-squares, and if
    \code{"symmetric"} a re-descending M estimator is used with Tukey's
    biweight function.}
  \item{method}{fit the model or just extract the model frame.}
  \item{control}{control parameters: see \code{\link{loess.control}}.}
  \item{\dots}{control parameters can also be supplied directly.}
}
\description{
  Fit a polynomial surface determined by one or more numerical
  predictors, using local fitting.
}
\details{
  Fitting is done locally.  That is, for the fit at point \eqn{x}, the
  fit is made using points in a neighbourhood of \eqn{x}, weighted by
  their distance from \eqn{x} (with differences in \sQuote{parametric}
  variables being ignored when computing the distance). The size of the
  neighbourhood is controlled by \eqn{\alpha} (set by \code{span} or
  \code{enp.target}).  For \eqn{\alpha < 1}, the neighbourhood includes
  proportion \eqn{\alpha} of the points, and these have tricubic weighting
  (proportional to \eqn{(1 - \mathrm{(dist/maxdist)}^3)^3}{(1 -
    (dist/maxdist)^3)^3}. For \eqn{\alpha > 1}, all points are used, with
  the \sQuote{maximum distance} assumed to be
  \eqn{\alpha^{1/p}}{alpha^1/p} times the actual maximum distance for
  \eqn{p} explanatory variables.

  For the default family, fitting is by (weighted) least squares. For
  \code{family="symmetric"} a few iterations of an M-estimation
  procedure with Tukey's biweight are used. Be aware that as the initial
  value is the least-squares fit, this need not be a very resistant fit.

  It can be important to tune the control list to achieve acceptable
  speed. See \code{\link{loess.control}} for details.
}
\value{
  An object of class \code{"loess"}.
}
\references{
  W.S. Cleveland, E. Grosse and W.M. Shyu (1992) Local regression
  models. Chapter 8 of \emph{Statistical Models in S} eds J.M. Chambers
  and T.J. Hastie, Wadsworth & Brooks/Cole.
}
\author{
  B.D. Ripley, based on the \code{cloess} package of Cleveland,
  Grosse and Shyu available at \url{http://www.netlib.org/a/}.
}
\note{
  As this is based on the \code{cloess} package available at
  \code{netlib}, it is similar to but not identical to the \code{loess}
  function of S. In particular, conditioning is not implemented.

  The memory usage of this implementation of \code{loess} is roughly
  quadratic in the number of points, with 1000 points taking about 10Mb.
}
\seealso{
  \code{\link{loess.control}},
  \code{\link{predict.loess}}.

  \code{\link{lowess}}, the ancestor of \code{loess} (with
  different defaults!).
}
\examples{
cars.lo <- loess(dist ~ speed, cars)
predict(cars.lo, data.frame(speed = seq(5, 30, 1)), se = TRUE)
# to allow extrapolation
cars.lo2 <- loess(dist ~ speed, cars,
  control = loess.control(surface = "direct"))
predict(cars.lo2, data.frame(speed = seq(5, 30, 1)), se = TRUE)
}
\keyword{smooth}
\keyword{loess}
