% File src/library/stats/man/NegBinomial.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2022 R Core Team
% Distributed under GPL 2 or later

\name{NegBinomial}
\alias{NegBinomial}
\alias{dnbinom}
\alias{pnbinom}
\alias{qnbinom}
\alias{rnbinom}
\title{The Negative Binomial Distribution}
\description{
  Density, distribution function, quantile function and random
  generation for the negative binomial distribution with parameters
  \code{size} and \code{prob}.
}
\usage{
dnbinom(x, size, prob, mu, log = FALSE)
pnbinom(q, size, prob, mu, lower.tail = TRUE, log.p = FALSE)
qnbinom(p, size, prob, mu, lower.tail = TRUE, log.p = FALSE)
rnbinom(n, size, prob, mu)
}
\arguments{
  \item{x}{vector of (non-negative integer) quantiles.}
  \item{q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations.  If \code{length(n) > 1}, the length
    is taken to be the number required.}
  \item{size}{target for number of successful trials, or dispersion
    parameter (the shape parameter of the gamma mixing distribution).
    Must be strictly positive, need not be integer.}
  \item{prob}{probability of success in each trial. \code{0 < prob <= 1}.}
  \item{mu}{alternative parametrization via mean: see \sQuote{Details}.}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]}, otherwise, \eqn{P[X > x]}.}
}
\details{
  The negative binomial distribution with \code{size} \eqn{= n} and
  \code{prob} \eqn{= p} has density
  \deqn{
    p(x) = \frac{\Gamma(x+n)}{\Gamma(n) x!} p^n (1-p)^x}{
    \Gamma(x+n)/(\Gamma(n) x!) p^n (1-p)^x}
  for \eqn{x = 0, 1, 2, \ldots}, \eqn{n > 0} and \eqn{0 < p \le 1}.

  This represents the number of failures which occur in a sequence of
  Bernoulli trials before a target number of successes is reached.
  The mean is \eqn{\mu = n(1-p)/p} and variance \eqn{n(1-p)/p^2}.

  A negative binomial distribution can also arise as a mixture of
  Poisson distributions with mean distributed as a gamma distribution
  (see \code{\link{pgamma}}) with scale parameter \code{(1 - prob)/prob}
  and shape parameter \code{size}.  (This definition allows non-integer
  values of \code{size}.)

  An alternative parametrization (often used in ecology) is by the
  \emph{mean} \code{mu} (see above), and \code{size}, the \emph{dispersion
  parameter}, where \code{prob} = \code{size/(size+mu)}.  The variance
  is \code{mu + mu^2/size} in this parametrization.

  If an element of \code{x} is not integer, the result of \code{dnbinom}
  is zero, with a warning.

  The case \code{size == 0} is the distribution concentrated at zero.
  This is the limiting distribution for \code{size} approaching zero,
  even if \code{mu} rather than \code{prob} is held constant.  Notice
  though, that the mean of the limit distribution is 0, whatever the
  value of \code{mu}.

  The quantile is defined as the smallest value \eqn{x} such that
  \eqn{F(x) \ge p}, where \eqn{F} is the distribution function.
}
\value{
  \code{dnbinom} gives the density,
  \code{pnbinom} gives the distribution function,
  \code{qnbinom} gives the quantile function, and
  \code{rnbinom} generates random deviates.

  Invalid \code{size} or \code{prob} will result in return value
  \code{NaN}, with a warning.

  The length of the result is determined by \code{n} for
  \code{rnbinom}, and is the maximum of the lengths of the
  numerical arguments for the other functions.

  The numerical arguments other than \code{n} are recycled to the
  length of the result.  Only the first elements of the logical
  arguments are used.

  \code{rnbinom} returns a vector of type \link{integer} unless generated
  values exceed the maximum representable integer when \code{\link{double}}
  values are returned.
}
\source{
  \code{dnbinom} computes via binomial probabilities, using code
  contributed by Catherine Loader (see \code{\link{dbinom}}).

  \code{pnbinom} uses \code{\link{pbeta}}.

  \code{qnbinom} uses the Cornish--Fisher Expansion to include a skewness
  correction to a normal approximation, followed by a search.

  \code{rnbinom} uses the derivation as a gamma mixture of Poisson
  distributions, see

  Devroye, L. (1986) \emph{Non-Uniform Random Variate Generation.}
  Springer-Verlag, New York. Page 480.
}
\seealso{
  \link{Distributions} for standard distributions, including
  \code{\link{dbinom}} for the binomial, \code{\link{dpois}} for the
  Poisson and \code{\link{dgeom}} for the geometric distribution, which
  is a special case of the negative binomial.
}
\examples{
require(graphics)
x <- 0:11
dnbinom(x, size = 1, prob = 1/2) * 2^(1 + x) # == 1
126 /  dnbinom(0:8, size  = 2, prob  = 1/2) #- theoretically integer

\donttest{## Cumulative ('p') = Sum of discrete prob.s ('d');  Relative error :
summary(1 - cumsum(dnbinom(x, size = 2, prob = 1/2)) /
                  pnbinom(x, size  = 2, prob = 1/2))}

x <- 0:15
size <- (1:20)/4
persp(x, size, dnb <- outer(x, size, function(x,s) dnbinom(x, s, prob = 0.4)),
      xlab = "x", ylab = "s", zlab = "density", theta = 150)
title(tit <- "negative binomial density(x,s, pr = 0.4)  vs.  x & s")

image  (x, size, log10(dnb), main = paste("log [", tit, "]"))
contour(x, size, log10(dnb), add = TRUE)

## Alternative parametrization
x1 <- rnbinom(500, mu = 4, size = 1)
x2 <- rnbinom(500, mu = 4, size = 10)
x3 <- rnbinom(500, mu = 4, size = 100)
h1 <- hist(x1, breaks = 20, plot = FALSE)
h2 <- hist(x2, breaks = h1$breaks, plot = FALSE)
h3 <- hist(x3, breaks = h1$breaks, plot = FALSE)
barplot(rbind(h1$counts, h2$counts, h3$counts),
        beside = TRUE, col = c("red","blue","cyan"),
        names.arg = round(h1$breaks[-length(h1$breaks)]))
}
\keyword{distribution}
