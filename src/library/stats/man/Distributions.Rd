% File src/library/stats/man/Distributions.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2010-2011 R Core Team
% Distributed under GPL 2 or later

\name{Distributions}
\alias{distribution}
\alias{distributions}
\alias{Distributions}
\title{Distributions in the stats package}
\description{
  Density, cumulative distribution function, quantile function and random
  variate generation for many standard probability distributions are
  available in the \pkg{stats} package.
}
\details{
  The functions for the density/mass function, cumulative distribution
  function, quantile function and random variate generation are named in the
  form \code{dxxx}, \code{pxxx}, \code{qxxx} and \code{rxxx} respectively.

  For the beta distribution see \code{\link{dbeta}}.

  For the binomial (including Bernoulli) distribution see
  \code{\link{dbinom}}.

  For the Cauchy distribution see \code{\link{dcauchy}}.

  For the chi-squared distribution see \code{\link{dchisq}}.

  For the exponential distribution see \code{\link{dexp}}.

  For the F distribution see \code{\link{df}}.

  For the gamma distribution see \code{\link{dgamma}}.

  For the geometric distribution see \code{\link{dgeom}}.  (This is also
  a special case of the negative binomial.)

  For the hypergeometric distribution see \code{\link{dhyper}}.

  For the log-normal distribution see \code{\link{dlnorm}}.

  For the multinomial distribution see \code{\link{dmultinom}}.

  For the negative binomial distribution see \code{\link{dnbinom}}.

  For the normal distribution see \code{\link{dnorm}}.

  For the Poisson distribution see \code{\link{dpois}}.

  For the Student's t distribution see \code{\link{dt}}.

  For the uniform distribution see \code{\link{dunif}}.

  For the Weibull distribution see \code{\link{dweibull}}.

  For less common distributions of test statistics see
  \code{\link{pbirthday}}, \code{\link{dsignrank}},
  \code{\link{ptukey}} and \code{\link{dwilcox}} (and see the
  \sQuote{See Also} section of \code{\link{cor.test}}).
}
\seealso{
  \code{\link{RNG}} about random number generation in \R.

  The CRAN task view on distributions,
  \url{https://cran.r-project.org/web/views/Distributions.html},
  mentioning several CRAN packages for additional distributions.
}
\keyword{distribution}
