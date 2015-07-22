% File src/library/stats/man/Fdist.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{FDist}
\alias{FDist}
\alias{df}
\alias{pf}
\alias{qf}
\alias{rf}
\title{The F Distribution}
\description{
  Density, distribution function, quantile function and random
  generation for the F distribution with \code{df1} and \code{df2}
  degrees of freedom (and optional non-centrality parameter \code{ncp}).
}
\usage{
df(x, df1, df2, ncp, log = FALSE)
pf(q, df1, df2, ncp, lower.tail = TRUE, log.p = FALSE)
qf(p, df1, df2, ncp, lower.tail = TRUE, log.p = FALSE)
rf(n, df1, df2, ncp)
}
\arguments{
  \item{x, q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations. If \code{length(n) > 1}, the length
    is taken to be the number required.}
  \item{df1, df2}{degrees of freedom.  \code{Inf} is allowed.}
  \item{ncp}{non-centrality parameter. If omitted the central F is assumed.}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]}, otherwise, \eqn{P[X > x]}.}
}
\value{
  \code{df} gives the density,
  \code{pf} gives the distribution function
  \code{qf} gives the quantile function, and
  \code{rf} generates random deviates.

  Invalid arguments will result in return value \code{NaN}, with a warning.

  The length of the result is determined by \code{n} for
  \code{rf}, and is the maximum of the lengths of the
  numerical arguments for the other functions.

  The numerical arguments other than \code{n} are recycled to the
  length of the result.  Only the first elements of the logical
  arguments are used.
}
\details{
  The F distribution with \code{df1 =} \eqn{n_1}{n1} and \code{df2 =}
  \eqn{n_2}{n2} degrees of freedom has density
  \deqn{
    f(x) = \frac{\Gamma(n_1/2 + n_2/2)}{\Gamma(n_1/2)\Gamma(n_2/2)}
    \left(\frac{n_1}{n_2}\right)^{n_1/2} x^{n_1/2 -1}
    \left(1 + \frac{n_1 x}{n_2}\right)^{-(n_1 + n_2) / 2}%
  }{f(x) = \Gamma((n1 + n2)/2) / (\Gamma(n1/2) \Gamma(n2/2))
    (n1/n2)^(n1/2) x^(n1/2 - 1)
    (1 + (n1/n2) x)^-(n1 + n2)/2}
  for \eqn{x > 0}.

  It is the distribution of the ratio of the mean squares of
  \eqn{n_1}{n1} and \eqn{n_2}{n2} independent standard normals, and hence
  of the ratio of two independent chi-squared variates each divided by its
  degrees of freedom.  Since the ratio of a normal and the root
  mean-square of \eqn{m} independent normals has a Student's \eqn{t_m}
  distribution, the square of a \eqn{t_m} variate has a F distribution on
  1 and \eqn{m} degrees of freedom.

  The non-central F distribution is again the ratio of mean squares of
  independent normals of unit variance, but those in the numerator are
  allowed to have non-zero means and \code{ncp} is the sum of squares of
  the means.  See \link{Chisquare} for further details on
  non-central distributions.
}
\source{
  For the central case of \code{df}, computed \emph{via} a binomial
  probability, code contributed by Catherine Loader (see
  \code{\link{dbinom}}); for the non-central case computed \emph{via}
  \code{\link{dbeta}}, code contributed by Peter Ruckdeschel.

  For \code{pf}, \emph{via} \code{\link{pbeta}} (or for large
  \code{df2}, \emph{via} \code{\link{pchisq}}).

  For \code{qf}, \emph{via} \code{\link{qchisq}} for large \code{df2},
  else \emph{via} \code{\link{qbeta}}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
  \emph{Continuous Univariate Distributions}, volume 2, chapters 27 and 30.
  Wiley, New York.
}
\note{
  Supplying \code{ncp = 0} uses the algorithm for the non-central
  distribution, which is not the same algorithm used if \code{ncp} is
  omitted.  This is to give consistent behaviour in extreme cases with
  values of \code{ncp} very near zero.

  The code for non-zero \code{ncp} is principally intended to be used
  for moderate values of \code{ncp}: it will not be highly accurate,
  especially in the tails, for large values.
}
\seealso{
  \link{Distributions} for other standard distributions, including
  \code{\link{dchisq}} for chi-squared and \code{\link{dt}} for Student's
  t distributions.
}
\examples{
## Equivalence of pt(.,nu) with pf(.^2, 1,nu):
x <- seq(0.001, 5, len = 100)
nu <- 4
stopifnot(all.equal(2*pt(x,nu) - 1, pf(x^2, 1,nu)),
          ## upper tails:
 	  all.equal(2*pt(x,     nu, lower=FALSE),
		      pf(x^2, 1,nu, lower=FALSE)))

## the density of the square of a t_m is 2*dt(x, m)/(2*x)
# check this is the same as the density of F_{1,m}
all.equal(df(x^2, 1, 5), dt(x, 5)/x)

## Identity:  qf(2*p - 1, 1, df)) == qt(p, df)^2)  for  p >= 1/2
p <- seq(1/2, .99, length = 50); df <- 10
rel.err <- function(x, y) ifelse(x == y, 0, abs(x-y)/mean(abs(c(x,y))))
\donttest{quantile(rel.err(qf(2*p - 1, df1 = 1, df2 = df), qt(p, df)^2), .90)  # ~= 7e-9}
}
\keyword{distribution}
