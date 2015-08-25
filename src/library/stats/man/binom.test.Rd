% File src/library/stats/man/binom.test.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
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
           conf.level = 0.95)
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
}
\details{
  Confidence intervals are obtained by a procedure first given in
  Clopper and Pearson (1934).  This guarantees that the confidence level
  is at least \code{conf.level}, but in general does not give the
  shortest-length confidence intervals.
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
  Clopper, C. J. & Pearson, E. S. (1934).
  The use of confidence or fiducial limits illustrated in the case of
  the binomial.
  \emph{Biometrika}, \bold{26}, 404--413.

  William J. Conover (1971),
  \emph{Practical nonparametric statistics}.
  New York: John Wiley & Sons.
  Pages 97--104.

  Myles Hollander & Douglas A. Wolfe (1973),
  \emph{Nonparametric Statistical Methods.}
  New York: John Wiley & Sons.
  Pages 15--22.
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
