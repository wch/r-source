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
  \item{n}{number of observations. If \code{length(n) > 1}, the length
    is taken to be the number required.}
  \item{size}{target for number of successful trials, or dispersion
    parameter (the shape parameter of the gamma mixing distribution).
    Must be strictly positive.}
  \item{prob}{probability of success in each trial. \code{0 < prob <= 1}.}
  \item{mu}{alternative parametrization via mean: see Details}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]}{P[X <= x]}, otherwise, \eqn{P[X > x]}{P[X > x]}.}
}
\details{
  The negative binomial distribution with \code{size} \eqn{= n} and
  \code{prob} \eqn{= p} has density
  \deqn{
    p(x) = \frac{\Gamma(x+n)}{\Gamma(n) x!} p^n (1-p)^x}{%
    p(x) = Gamma(x+n)/(Gamma(n) x!) p^n (1-p)^x}
  for \eqn{x = 0, 1, 2, \ldots}, \eqn{n > 0} and \eqn{0 < p \le 1}.

  This represents the number of failures which occur in a sequence of
  Bernoulli trials before a target number of successes is reached.

  A negative binomial distribution can arise as a mixture of Poisson
  distributions with mean distributed as a
  \eqn{\Gamma} (\code{\link{pgamma}}) distribution with scale parameter
  \code{(1 - prob)/prob} and shape parameter \code{size}.  (This
  definition allows non-integer values of \code{size}.)
  In this model \code{prob} = \code{scale/(1+scale)}, and the mean is
  \code{size * (1 - prob)/prob}.

  The alternative parametrization (often used in ecology) is by the
  \emph{mean} \code{mu}, and \code{size}, the \emph{dispersion parameter},
  where \code{prob} = \code{size/(size+mu)}.
  The variance is \code{mu + mu^2/size} in this parametrization or
  \eqn{n (1-p)/p^2} in the first one.

  If an element of \code{x} is not integer, the result of \code{dnbinom}
  is zero, with a warning.

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
}
\source{
  \code{dnbinom} computes via binomial probabilities, using code
  contributed by Catherine Loader (see \code{\link{dbinom}}).

  \code{pnbinom} uses \code{\link{pbeta}}.

  \code{qnbinom} uses the Cornish--Fisher Expansion to include a skewness
  correction to a normal approximation, followed by a search.

  \code{rnbinom} uses the derivation as a gamma mixture of Poissons, see

  Devroye, L. (1986) \emph{Non-Uniform Random Variate Generation.}
  Springer-Verlag, New York. Page 480.
}
\seealso{
  \code{\link{dbinom}} for the binomial, \code{\link{dpois}} for the
  Poisson and \code{\link{dgeom}} for the geometric distribution, which
  is a special case of the negative binomial.
}
\examples{
x <- 0:11
dnbinom(x, size = 1, prob = 1/2) * 2^(1 + x) # == 1
126 /  dnbinom(0:8, size  = 2, prob  = 1/2) #- theoretically integer

## Cumulative ('p') = Sum of discrete prob.s ('d');  Relative error :
summary(1 - cumsum(dnbinom(x, size = 2, prob = 1/2)) /
                  pnbinom(x, size  = 2, prob = 1/2))

x <- 0:15
size <- (1:20)/4
persp(x,size, dnb <- outer(x,size,function(x,s)dnbinom(x,s, pr= 0.4)),
      xlab = "x", ylab = "s", zlab="density", theta = 150)
title(tit <- "negative binomial density(x,s, pr = 0.4)  vs.  x & s")

image  (x,size, log10(dnb), main= paste("log [",tit,"]"))
contour(x,size, log10(dnb),add=TRUE)

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
