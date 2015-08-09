% File src/library/stats/man/prop.test.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2010 R Core Team
% Distributed under GPL 2 or later

\name{prop.test}
\title{Test of Equal or Given Proportions}
\usage{
prop.test(x, n, p = NULL,
          alternative = c("two.sided", "less", "greater"),
          conf.level = 0.95, correct = TRUE)
}
\alias{prop.test}
\arguments{
  \item{x}{a vector of counts of successes, a one-dimensional table with
    two entries, or a two-dimensional table (or matrix) with 2 columns,
    giving the counts of successes and failures, respectively.}
  \item{n}{a vector of counts of trials; ignored if \code{x} is a
    matrix or a table.}
  \item{p}{a vector of probabilities of success.  The length of
    \code{p} must be the same as the number of groups specified by
    \code{x}, and its elements must be greater than 0 and less than 1.}
  \item{alternative}{a character string specifying the alternative
    hypothesis, must be one of \code{"two.sided"} (default),
    \code{"greater"} or \code{"less"}.  You can specify just the initial
    letter.  Only used for testing the null that a single proportion
    equals a given value, or that two proportions are equal; ignored
    otherwise.}
  \item{conf.level}{confidence level of the returned confidence
    interval.  Must be a single number between 0 and 1.  Only used
    when testing the null that a single proportion equals a given
    value, or that two proportions are equal; ignored otherwise.}
  \item{correct}{a logical indicating whether Yates' continuity
    correction should be applied where possible.}
}
\description{
  \code{prop.test} can be used for testing the null that the
  proportions (probabilities of success) in several groups are the
  same, or that they equal certain given values.
}
\details{
  Only groups with finite numbers of successes and failures are used.
  Counts of successes and failures must be nonnegative and hence not
  greater than the corresponding numbers of trials which must be
  positive.  All finite counts should be integers.

  If \code{p} is \code{NULL} and there is more than one group, the null
  tested is that the proportions in each group are the same.  If there
  are two groups, the alternatives are that the probability of success
  in the first group is less than, not equal to, or greater than the
  probability of success in the second group, as specified by
  \code{alternative}.  A confidence interval for the difference of
  proportions with confidence level as specified by \code{conf.level}
  and clipped to \eqn{[-1,1]} is returned.  Continuity correction is
  used only if it does not exceed the difference of the sample
  proportions in absolute value.  Otherwise, if there are more than 2
  groups, the alternative is always \code{"two.sided"}, the returned
  confidence interval is \code{NULL}, and continuity correction is never
  used.

  If there is only one group, then the null tested is that the
  underlying probability of success is \code{p}, or .5 if \code{p} is
  not given.  The alternative is that the probability of success is less
  than, not equal to, or greater than \code{p} or 0.5, respectively, as
  specified by \code{alternative}.  A confidence interval for the
  underlying proportion with confidence level as specified by
  \code{conf.level} and clipped to \eqn{[0,1]} is returned.  Continuity
  correction is used only if it does not exceed the difference between
  sample and null proportions in absolute value. The confidence interval
  is computed by inverting the score test.

  Finally, if \code{p} is given and there are more than 2 groups, the
  null tested is that the underlying probabilities of success are those
  given by \code{p}.  The alternative is always \code{"two.sided"}, the
  returned confidence interval is \code{NULL}, and continuity correction
  is never used.
}
\value{
  A list with class \code{"htest"} containing the following
  components:
  \item{statistic}{the value of Pearson's chi-squared test statistic.}
  \item{parameter}{the degrees of freedom of the approximate
    chi-squared distribution of the test statistic.}
  \item{p.value}{the p-value of the test.}
  \item{estimate}{a vector with the sample proportions \code{x/n}.}
  \item{conf.int}{a confidence interval for the true proportion if
    there is one group, or for the difference in proportions if
    there are 2 groups and \code{p} is not given, or \code{NULL}
    otherwise.  In the cases where it is not \code{NULL}, the
    returned confidence interval has an asymptotic confidence level
    as specified by \code{conf.level}, and is appropriate to the
    specified alternative hypothesis.}
  \item{null.value}{the value of \code{p} if specified by the null, or
    \code{NULL} otherwise.}
  \item{alternative}{a character string describing the alternative.}
  \item{method}{a character string indicating the method used, and
    whether Yates' continuity correction was applied.}
  \item{data.name}{a character string giving the names of the data.}
}
\references{
  Wilson, E.B. (1927) Probable inference, the law of succession, and
  statistical inference.
  \emph{J. Am. Stat. Assoc.}, \bold{22}, 209--212.

  Newcombe R.G. (1998) Two-Sided Confidence Intervals for the Single
  Proportion: Comparison of Seven Methods.
  \emph{Statistics in Medicine} \bold{17}, 857--872.

  Newcombe R.G. (1998) Interval Estimation for the Difference Between
  Independent Proportions: Comparison of Eleven Methods.
  \emph{Statistics in Medicine} \bold{17}, 873--890.
}
\seealso{\code{\link{binom.test}} for an \emph{exact} test of a binomial
  hypothesis.}
\examples{
heads <- rbinom(1, size = 100, prob = .5)
prop.test(heads, 100)          # continuity correction TRUE by default
prop.test(heads, 100, correct = FALSE)

## Data from Fleiss (1981), p. 139.
## H0: The null hypothesis is that the four populations from which
##     the patients were drawn have the same true proportion of smokers.
## A:  The alternative is that this proportion is different in at
##     least one of the populations.

smokers  <- c( 83, 90, 129, 70 )
patients <- c( 86, 93, 136, 82 )
prop.test(smokers, patients)
}
\keyword{htest}
