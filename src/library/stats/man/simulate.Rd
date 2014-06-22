% File src/library/stats/man/simulate.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{simulate}
\title{Simulate Responses}
\description{
  Simulate one or more responses from the distribution
  corresponding to a fitted model object.
}
\usage{
simulate(object, nsim = 1, seed = NULL, \dots)
}
\alias{simulate}
\arguments{
  \item{object}{an object representing a fitted model.}
  \item{nsim}{number of response vectors to simulate.  Defaults to \code{1}.}
  \item{seed}{an object specifying if and how the random number
    generator should be initialized (\sQuote{seeded}).\cr
    For the "lm" method, either \code{NULL} or an integer that will be
    used in a call to \code{set.seed} before simulating the response
    vectors.  If set, the value is saved as the \code{"seed"} attribute
    of the returned value.  The default, \code{NULL} will not change the
    random generator state, and return \code{\link{.Random.seed}} as the
    \code{"seed"} attribute, see \sQuote{Value}.
  }
  \item{\dots}{additional optional arguments.}
}
\value{
  Typically, a list of length \code{nsim} of simulated responses.  Where
  appropriate the result can be a data frame (which is a special type of
  list).
  %% a *matrix* seems very natural and is more efficient
  %% for large-scale simulation, already for stats:::simulate.lm (in ../R/lm.R )

  For the \code{"lm"} method, the result is a data frame with an
  attribute \code{"seed"}.  If argument \code{seed} is \code{NULL}, the
  attribute is the value of \code{\link{.Random.seed}} before the
  simulation was started; otherwise it is the value of the argument with
  a \code{"kind"} attribute with value \code{as.list(\link{RNGkind}())}.
}
\details{
  This is a generic function.  Consult the individual modeling functions
  for details on how to use this function.

  Package \pkg{stats} has a method for \code{"lm"} objects which is used
  for \code{\link{lm}} and \code{\link{glm}} fits.  There is a method
  for fits from \code{glm.nb} in package \CRANpkg{MASS}, and hence the
  case of negative binomial families is not covered by the \code{"lm"}
  method.

  The methods for linear models fitted by \code{lm} or \code{glm(family
  = "gaussian")} assume that any weights which have been supplied are
  inversely proportional to the error variance.  For other GLMs the
  (optional) \code{simulate} component of the \code{\link{family}}
  object is used---there is no appropriate simulation method for
  \sQuote{quasi} models as they are specified only up to two moments.

  For binomial and Poisson GLMs the dispersion is fixed at one.  Integer
  prior weights \eqn{w_i} can be interpreted as meaning that
  observation \eqn{i} is an average of \eqn{w_i} observations, which is
  natural for binomials specified as proportions but less so for a
  Poisson, for which prior weights are ignored with a warning.

  For a gamma GLM the shape parameter is estimated by maximum likelihood
  (using function \code{\link[MASS:gamma.shape.glm]{gamma.shape}} in package
  \CRANpkg{MASS}).  The interpretation of weights is as multipliers to a
  basic shape parameter, since dispersion is inversely proportional to
  shape.

  For an inverse gaussian GLM the model assumed is
  \eqn{IG(\mu_i, \lambda w_i)} (see
  \url{http://en.wikipedia.org/wiki/Inverse_Gaussian_distribution})
  where \eqn{\lambda} is estimated by the inverse of the dispersion
  estimate for the fit.  The variance is
  \eqn{\mu_i^3/(\lambda w_i)} and
  hence inversely proportional to the prior weights.  The simulation is
  done by function \code{\link[SuppDists:invGauss]{rinvGauss}} from the
  \CRANpkg{SuppDists} package, which must be installed.
}
\seealso{
  \code{\link{RNG}} about random number generation in \R,
  \code{\link{fitted.values}} and \code{\link{residuals}} for related methods;
  \code{\link{glm}}, \code{\link{lm}} for model fitting.

  There are further examples in the \file{simulate.R} tests file in the
  sources for package \pkg{stats}.
}
\examples{
x <- 1:5
mod1 <- lm(c(1:3, 7, 6) ~ x)
S1 <- simulate(mod1, nsim = 4)
## repeat the simulation:
.Random.seed <- attr(S1, "seed")
identical(S1, simulate(mod1, nsim = 4))

S2 <- simulate(mod1, nsim = 200, seed = 101)
rowMeans(S2) # should be about the same as
fitted(mod1)

## repeat identically:
(sseed <- attr(S2, "seed")) # seed; RNGkind as attribute
stopifnot(identical(S2, simulate(mod1, nsim = 200, seed = sseed)))

## To be sure about the proper RNGkind, e.g., after
RNGversion("2.7.0")
## first set the RNG kind, then simulate
do.call(RNGkind, attr(sseed, "kind"))
identical(S2, simulate(mod1, nsim = 200, seed = sseed))

## Binomial GLM examples
yb1 <- matrix(c(4, 4, 5, 7, 8, 6, 6, 5, 3, 2), ncol = 2)
modb1 <- glm(yb1 ~ x, family = binomial)
S3 <- simulate(modb1, nsim = 4)
# each column of S3 is a two-column matrix.

x2 <- sort(runif(100))
yb2 <- rbinom(100, prob = plogis(2*(x2-1)), size = 1)
yb2 <- factor(1 + yb2, labels = c("failure", "success"))
modb2 <- glm(yb2 ~ x2, family = binomial)
S4 <- simulate(modb2, nsim = 4)
# each column of S4 is a factor
}
\keyword{models}
\keyword{datagen}
