% File src/library/stats/man/binom.test.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2026 R Core Team
% Distributed under GPL 2 or later

\name{binom.test}
\alias{binom.test}
\title{Exact Binomial Test}
\description{
  Performs an exact test of a simple null hypothesis about the
  probability of success in a Bernoulli experiment.
}
\usage{
binom.test(x, n, p = 0.5,
           alternative = c("two.sided", "less", "greater"),
           conf.level = 0.95, two.sided.method = c("minlike", "central"))
}
\arguments{
  \item{x}{number of successes, or a vector of length 2 giving the
    numbers of successes and failures, respectively.}
  \item{n}{number of trials; ignored if \code{x} has length 2.}
  \item{p}{hypothesized probability of success.}
  \item{alternative}{indicates the alternative hypothesis and must be
    one of \code{"two.sided"}, \code{"greater"} or \code{"less"}.
    You can specify just the initial letter.}
  \item{conf.level}{confidence level for the returned confidence
    interval.}
  \item{two.sided.method}{a character string specifying the method for
    computing two-sided p-values, must be one of \code{"minlike"}
    (default) or \code{"central"}, or an abbrevation thereof.
    See \sQuote{Details}.}
}
\details{
  Confidence intervals are obtained by a procedure first given in
  \bibcitet{R:Clopper+Pearson:1934}.
  This guarantees that the confidence level
  is at least \code{conf.level}, but in general does not give the
  shortest-length confidence intervals.

  Suppose \eqn{T} is a statistic for testing hypothesis about a scalar
  parameter \eqn{\theta} such that \eqn{P(T \le t | \theta)} is
  increasing in \eqn{\theta}.
  For testing the null that \eqn{\theta = \theta_0} against the
  two-sided alternative that \eqn{\theta \ne \theta_0}, the 
  \emph{central} p-value \bibcitep{R:Fay:2010} is
  \deqn{\min(2 \min(P(T \le t | \theta_0), P(T \ge t | \theta_0)), 1)}
  (this is called the twice the smaller tail method in
  \bibcitet{R:Hirji:2005}).
  The \emph{minlike} p-value is the probability of observing a value of
  \eqn{T} not more likely than the one observed, i.e., 
  \deqn{\sum_{s: P(T = s | \theta_0) \le P(T = s | \theta_0)} P(T = s | \theta_0)}
  (this is called the probability based method by Hirji).
  
  For the exact binomial test, \eqn{T} is the number of successes and
  \eqn{\theta = p} is the probability of success.

  Inverting the two-sided test which rejects when the central p-value is
  at most \eqn{\alpha} gives central confidence intervals with
  confidence level at least \eqn{1 - \alpha} where both lower and upper
  tails have probability at most \eqn{\alpha / 2}.  For the exact
  binomial test, these are the Clopper-Pearson intervals.

  It is intended to change the default method for computing two-sided
  p-values from minlike to central to make these match the confidence
  intervals.
}
\value{
  A list with class \code{"htest"} containing the following components:
  \item{statistic}{the number of successes.}
  \item{parameter}{the number of trials.}
  \item{p.value}{the p-value of the test.}
  \item{conf.int}{a confidence interval for the probability of success.}
  \item{estimate}{the estimated probability of success.}
  \item{null.value}{the probability of success under the null,
    \code{p}.}
  \item{alternative}{a character string describing the alternative
    hypothesis.}
  \item{method}{the character string \code{"Exact binomial test"}.}
  \item{data.name}{a character string giving the names of the data.}
}
\references{
  \bibinfo{R:Conover:1971}{footer}{Pages 97--104.}
  \bibinfo{R:Hollander+Wolfe:1973}{footer}{Pages 15--22.}
  \bibshow{*, R:Conover:1971, R:Hollander+Wolfe:1973}
}
\seealso{
  \code{\link{prop.test}} for a general (approximate) test for equal or
  given proportions.
}
\examples{
## Conover (1971), p. 97f.
## Under (the assumption of) simple Mendelian inheritance, a cross
##  between plants of two particular genotypes produces progeny 1/4 of
##  which are "dwarf" and 3/4 of which are "giant", respectively.
##  In an experiment to determine if this assumption is reasonable, a
##  cross results in progeny having 243 dwarf and 682 giant plants.
##  If "giant" is taken as success, the null hypothesis is that p =
##  3/4 and the alternative that p != 3/4.
binom.test(c(682, 243), p = 3/4)
binom.test(682, 682 + 243, p = 3/4)   # The same.
## => Data are in agreement with the null hypothesis.
}
\keyword{htest}
