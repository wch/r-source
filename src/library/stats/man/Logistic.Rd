% File src/library/stats/man/Logistic.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{Logistic}
\alias{Logistic}
\alias{dlogis}
\alias{plogis}
\alias{qlogis}
\alias{rlogis}
\title{The Logistic Distribution}
\concept{logit}
\concept{sigmoid}
\description{
  Density, distribution function, quantile function and random
  generation for the logistic distribution with parameters
  \code{location} and \code{scale}.
}
\usage{
dlogis(x, location = 0, scale = 1, log = FALSE)
plogis(q, location = 0, scale = 1, lower.tail = TRUE, log.p = FALSE)
qlogis(p, location = 0, scale = 1, lower.tail = TRUE, log.p = FALSE)
rlogis(n, location = 0, scale = 1)
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
  \code{dlogis} gives the density,
  \code{plogis} gives the distribution function,
  \code{qlogis} gives the quantile function, and
  \code{rlogis} generates random deviates.
  
  The length of the result is determined by \code{n} for
  \code{rlogis}, and is the maximum of the lengths of the
  numerical arguments for the other functions.  
  
  The numerical arguments other than \code{n} are recycled to the
  length of the result.  Only the first elements of the logical
  arguments are used.
}
\details{
  If \code{location} or \code{scale} are omitted, they assume the
  default values of \code{0} and \code{1} respectively.

  The Logistic distribution with \code{location} \eqn{= \mu}{= m} and
  \code{scale} \eqn{= \sigma}{= s} has distribution function
  \deqn{
    F(x) = \frac{1}{1 + e^{-(x-\mu)/\sigma}}%
  }{F(x) = 1 / (1 + exp(-(x-m)/s))}  and density
  \deqn{
    f(x)= \frac{1}{\sigma}\frac{e^{(x-\mu)/\sigma}}{(1 + e^{(x-\mu)/\sigma})^2}%
  }{f(x) = 1/s exp((x-m)/s) (1 + exp((x-m)/s))^-2.}

  It is a long-tailed distribution with mean \eqn{\mu}{m} and variance
  \eqn{\pi^2/3 \sigma^2}{\pi^2 /3 s^2}.
}
\note{
  \code{qlogis(p)} is the same as the well known \sQuote{\emph{logit}}
  function, \eqn{logit(p) = \log p/(1-p)}{logit(p) = log(p/(1-p))},
  and \code{plogis(x)} has consequently been called the \sQuote{inverse logit}.

  The distribution function is a rescaled hyperbolic tangent,
  \code{plogis(x) == (1+ \link{tanh}(x/2))/2}, and it is called a
  \emph{sigmoid function} in contexts such as neural networks.
}
\source{
  \code{[dpq]logis} are calculated directly from the definitions.

  \code{rlogis} uses inversion.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
  \emph{Continuous Univariate Distributions}, volume 2, chapter 23.
  Wiley, New York.
}
\seealso{
  \link{Distributions} for other standard distributions.
}
\examples{
var(rlogis(4000, 0, scale = 5))  # approximately (+/- 3)
pi^2/3 * 5^2
}
\keyword{distribution}
