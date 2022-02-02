% File src/library/stats/man/poisson.test.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{poisson.test}
\alias{poisson.test}
\title{Exact Poisson tests}
\description{
     Performs an exact test of a simple null hypothesis about the
     rate parameter in Poisson distribution, or for the
     ratio between two rate parameters.
}
\usage{
poisson.test(x, T = 1, r = 1,
    alternative = c("two.sided", "less", "greater"),
    conf.level = 0.95)
}
\arguments{
  \item{x}{number of events. A vector of length one or two.}
  \item{T}{time base for event count. A vector of length one or two. }
  \item{r}{hypothesized rate or rate ratio}
  \item{alternative}{indicates the alternative hypothesis and must be
    one of \code{"two.sided"}, \code{"greater"} or \code{"less"}.
    You can specify just the initial letter.}
  \item{conf.level}{confidence level for the returned confidence
    interval.}
}
\details{
  Confidence intervals are computed similarly to those of
  \code{\link{binom.test}} in the one-sample case, and using
  \code{\link{binom.test}} in the two sample case.
}
\value{
  A list with class \code{"htest"} containing the following components:
  \item{statistic}{the number of events (in the first sample if there
    are two.)}
  \item{parameter}{the corresponding expected count}
  \item{p.value}{the p-value of the test.}
  \item{conf.int}{a confidence interval for the rate or rate ratio.}
  \item{estimate}{the estimated rate or rate ratio.}
  \item{null.value}{the rate or rate ratio under the null,
    \code{r}.}
  \item{alternative}{a character string describing the alternative
    hypothesis.}
  \item{method}{the character string \code{"Exact Poisson test"} or
    \code{"Comparison of Poisson rates"} as appropriate.}
  \item{data.name}{a character string giving the names of the data.}
}
\note{
  The rate parameter in Poisson data is often given based on a
  \dQuote{time on test} or similar quantity (person-years, population
  size, or expected number of cases from mortality tables). This is the
  role of the \code{T} argument.

  The one-sample case is effectively the binomial test with a very large
  \code{n}. The two sample case is converted to a binomial test by
  conditioning on the total event count, and the rate ratio is directly
  related to the odds in that binomial distribution.
}
\seealso{
  \code{\link{binom.test}}
}
\examples{
### These are paraphrased from data sets in the ISwR package

## SMR, Welsh Nickel workers
poisson.test(137, 24.19893)

## eba1977, compare Fredericia to other three cities for ages 55-59
poisson.test(c(11, 6+8+7), c(800, 1083+1050+878))
}
\keyword{htest}

