% File src/library/stats/man/Cauchy.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{Cauchy}
\alias{Cauchy}
\alias{dcauchy}
\alias{pcauchy}
\alias{qcauchy}
\alias{rcauchy}
\title{The Cauchy Distribution}
\description{
  Density, distribution function, quantile function and random
  generation for the Cauchy distribution with location parameter
  \code{location} and scale parameter \code{scale}.
}
\usage{
dcauchy(x, location = 0, scale = 1, log = FALSE)
pcauchy(q, location = 0, scale = 1, lower.tail = TRUE, log.p = FALSE)
qcauchy(p, location = 0, scale = 1, lower.tail = TRUE, log.p = FALSE)
rcauchy(n, location = 0, scale = 1)
}
\arguments{
  \item{x, q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations. If \code{length(n) > 1}, the length
    is taken to be the number required.}
  \item{location, scale}{location and scale parameters.}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]}, otherwise, \eqn{P[X > x]}.}
}
\value{
  \code{dcauchy}, \code{pcauchy}, and \code{qcauchy} are respectively
  the density, distribution function and quantile function of the Cauchy
  distribution.  \code{rcauchy} generates random deviates from the
  Cauchy.
  
  The length of the result is determined by \code{n} for
  \code{rcauchy}, and is the maximum of the lengths of the
  numerical arguments for the other functions.  
  
  The numerical arguments other than \code{n} are recycled to the
  length of the result.  Only the first elements of the logical
  arguments are used.    
}
\details{
  If \code{location} or \code{scale} are not specified, they assume
  the default values of \code{0} and \code{1} respectively.

  The Cauchy distribution with location \eqn{l} and scale \eqn{s} has
  density
  \deqn{f(x) = \frac{1}{\pi s}
    \left( 1 + \left(\frac{x - l}{s}\right)^2 \right)^{-1}%
  }{f(x) = 1 / (\pi s (1 + ((x-l)/s)^2))}
  for all \eqn{x}.
}
\source{
  \code{dcauchy}, \code{pcauchy} and \code{qcauchy} are all calculated
  from numerically stable versions of the definitions.

  \code{rcauchy} uses inversion.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
  \emph{Continuous Univariate Distributions}, volume 1, chapter 16.
  Wiley, New York.
}
\seealso{
  \link{Distributions} for other standard distributions, including
  \code{\link{dt}} for the t distribution which generalizes
  \code{dcauchy(*, l = 0, s = 1)}.
}
\examples{
dcauchy(-1:4)
}
\keyword{distribution}
