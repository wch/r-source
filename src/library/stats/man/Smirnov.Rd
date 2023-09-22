% File src/library/stats/man/Smirnov.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2022-2023 R Core Team
% Distributed under GPL 2 or later

\name{Smirnov}
\alias{Smirnov}
\alias{psmirnov}
\alias{qsmirnov}
\alias{rsmirnov}
\title{Distribution of the Smirnov Statistic}
\description{
  Distribution function, quantile function and random generation for the
  distribution of the Smirnov statistic.}
\usage{
psmirnov(q, sizes, z = NULL,
         alternative = c("two.sided", "less", "greater"),
         exact = TRUE, simulate = FALSE, B = 2000,
         lower.tail = TRUE, log.p = FALSE)
qsmirnov(p, sizes, z = NULL,
         alternative = c("two.sided", "less", "greater"),
         exact = TRUE, simulate = FALSE, B = 2000)
rsmirnov(n, sizes, z = NULL,
         alternative = c("two.sided", "less", "greater")) 
}
\arguments{
  \item{q}{a numeric vector of quantiles.}
  \item{p}{a numeric vector of probabilities.}
  \item{sizes}{an integer vector of length two giving the sample sizes.}
  \item{z}{a numeric vector of the pooled data values in both samples
    when the exact conditional distribution of the Smirnov statistic
    given the data shall be computed.}
  \item{alternative}{one of \code{"two.sided"} (default), \code{"less"},
    or \code{"greater"} indicating whether absolute (two-sided, default)
    or raw (one-sided) differences of frequencies define the test
    statistic.  See \sQuote{Details}.}
  \item{exact}{\code{NULL} or a logical indicating whether the exact
    (conditional on the pooled data values in \code{z}) distribution
    or the asymptotic distribution should be used.}
  \item{simulate}{a logical indicating whether to compute the
    distribution function by Monte Carlo simulation.}
  \item{B}{an integer specifying the number of replicates used in the
    Monte Carlo test.}
  \item{lower.tail}{a logical, if \code{TRUE} (default), probabilities are
    \eqn{P[D < q]}, otherwise, \eqn{P[D \ge q]}.}
  \item{log.p}{a logical, if \code{TRUE} (default), probabilities are given
    as log-probabilities.}
  \item{n}{an integer giving number of observations.}
}
\value{
  \code{psmirnov} gives the distribution function,
  \code{qsmirnov} gives the quantile function, and
  \code{rsmirnov} generates random deviates.
}
\details{
  For samples \eqn{x} and \eqn{y} with respective sizes \eqn{n_x} and
  \eqn{n_y} and empirical cumulative distribution functions
  \eqn{F_{x,n_x}} and \eqn{F_{y,n_y}}, the Smirnov statistic is
  \deqn{D = \sup_c | F_{x,n_x}(c) - F_{y,n_y}(c) |}
  in the two-sided case,
  \deqn{D^+ = \sup_c ( F_{x,n_x}(c) - F_{y,n_y}(c) )}
  in the one-sided \code{"greater"} case, and
  \deqn{D^- = \sup_c ( F_{y,n_y}(c) - F_{x,n_x}(c) )}
  in the one-sided \code{"less"} case.

  These statistics are used in the Smirnov test of the null that \eqn{x}
  and \eqn{y} were drawn from the same distribution, see
  \code{\link{ks.test}}.

  If the underlying common distribution function \eqn{F} is continuous,
  the distribution of the test statistics does not depend on \eqn{F},
  and has a simple asymptotic approximation.  For arbitrary \eqn{F}, one
  can compute the conditional distribution given the pooled data values
  \eqn{z} of \eqn{x} and \eqn{y}, either exactly (feasible provided that
  the product \eqn{n_x n_y} of the sample sizes is ``small enough'') or
  approximately Monte Carlo simulation. If the pooled data values \eqn{z}
  are not specified, a pooled sample without ties is assumed.
}
\seealso{
  \code{\link{ks.test}} for references on the algorithms used for
  computing exact distributions.
}
