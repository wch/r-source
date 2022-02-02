% File src/library/stats/man/mantelhaen.test.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2008 R Core Team
% Distributed under GPL 2 or later

\name{mantelhaen.test}
\alias{mantelhaen.test}
\title{Cochran-Mantel-Haenszel Chi-Squared Test for Count Data}
\description{
  Performs a Cochran-Mantel-Haenszel chi-squared test of the null that
  two nominal variables are conditionally independent in each stratum,
  assuming that there is no three-way interaction.
}
\usage{
mantelhaen.test(x, y = NULL, z = NULL,
                alternative = c("two.sided", "less", "greater"),
                correct = TRUE, exact = FALSE, conf.level = 0.95)
}
\arguments{
  \item{x}{either a 3-dimensional contingency table in array form where
    each dimension is at least 2 and the last dimension corresponds to
    the strata, or a factor object with at least 2 levels.}
  \item{y}{a factor object with at least 2 levels; ignored if \code{x}
    is an array.}
  \item{z}{a factor object with at least 2 levels identifying to which
    stratum the corresponding elements in \code{x} and \code{y} belong;
    ignored if \code{x} is an array.}
  \item{alternative}{indicates the alternative hypothesis and must be
    one of \code{"two.sided"}, \code{"greater"} or \code{"less"}.
    You can specify just the initial letter.
    Only used in the 2 by 2 by \eqn{K} case.}
  \item{correct}{a logical indicating whether to apply continuity
    correction when computing the test statistic.
    Only used in the 2 by 2 by \eqn{K} case.}
  \item{exact}{a logical indicating whether the Mantel-Haenszel test or
    the exact conditional test (given the strata margins) should be
    computed.
    Only used in the 2 by 2 by \eqn{K} case.}
  \item{conf.level}{confidence level for the returned confidence
    interval.
    Only used in the 2 by 2 by \eqn{K} case.}
}
\value{
  A list with class \code{"htest"} containing the following components:
  \item{statistic}{Only present if no exact test is performed.  In the
    classical case of a 2 by 2 by \eqn{K} table (i.e., of dichotomous
    underlying variables), the Mantel-Haenszel chi-squared statistic;
    otherwise, the generalized Cochran-Mantel-Haenszel statistic.}
  \item{parameter}{the degrees of freedom of the approximate chi-squared
    distribution of the test statistic (\eqn{1} in the classical case).
    Only present if no exact test is performed.}
  \item{p.value}{the p-value of the test.}
  \item{conf.int}{a confidence interval for the common odds ratio.
    Only present in the 2 by 2 by \eqn{K} case.}
  \item{estimate}{an estimate of the common odds ratio.  If an exact
    test is performed, the conditional Maximum Likelihood Estimate is
    given; otherwise, the Mantel-Haenszel estimate.
    Only present in the 2 by 2 by \eqn{K} case.}
  \item{null.value}{the common odds ratio under the null of
    independence, \code{1}.
    Only present in the 2 by 2 by \eqn{K} case.}
  \item{alternative}{a character string describing the alternative
    hypothesis.
    Only present in the 2 by 2 by \eqn{K} case.}
  \item{method}{a character string indicating the method employed, and whether
    or not continuity correction was used.}
  \item{data.name}{a character string giving the names of the data.}
}
\details{
  If \code{x} is an array, each dimension must be at least 2, and
  the entries should be nonnegative integers.  \code{NA}'s are not
  allowed.  Otherwise, \code{x}, \code{y} and \code{z} must have the
  same length.  Triples containing \code{NA}'s are removed.  All
  variables must take at least two different values.
}
\note{
  The asymptotic distribution is only valid if there is no three-way
  interaction.  In the classical 2 by 2 by \eqn{K} case, this is
  equivalent to the conditional odds ratios in each stratum being
  identical.  Currently, no inference on homogeneity of the odds ratios
  is performed.

  See also the example below.
}
\references{
  Alan Agresti (1990).
  \emph{Categorical data analysis}.
  New York: Wiley.
  Pages 230--235.

  Alan Agresti (2002).
  \emph{Categorical data analysis} (second edition).
  New York: Wiley.
}
\examples{
## Agresti (1990), pages 231--237, Penicillin and Rabbits
## Investigation of the effectiveness of immediately injected or 1.5
##  hours delayed penicillin in protecting rabbits against a lethal
##  injection with beta-hemolytic streptococci.
Rabbits <-
array(c(0, 0, 6, 5,
        3, 0, 3, 6,
        6, 2, 0, 4,
        5, 6, 1, 0,
        2, 5, 0, 0),
      dim = c(2, 2, 5),
      dimnames = list(
          Delay = c("None", "1.5h"),
          Response = c("Cured", "Died"),
          Penicillin.Level = c("1/8", "1/4", "1/2", "1", "4")))
Rabbits
## Classical Mantel-Haenszel test
mantelhaen.test(Rabbits)
## => p = 0.047, some evidence for higher cure rate of immediate
##               injection
## Exact conditional test
mantelhaen.test(Rabbits, exact = TRUE)
## => p - 0.040
## Exact conditional test for one-sided alternative of a higher
## cure rate for immediate injection
mantelhaen.test(Rabbits, exact = TRUE, alternative = "greater")
## => p = 0.020

## UC Berkeley Student Admissions
mantelhaen.test(UCBAdmissions)
## No evidence for association between admission and gender
## when adjusted for department.  However,
apply(UCBAdmissions, 3, function(x) (x[1,1]*x[2,2])/(x[1,2]*x[2,1]))
## This suggests that the assumption of homogeneous (conditional)
## odds ratios may be violated.  The traditional approach would be
## using the Woolf test for interaction:
woolf <- function(x) {
  x <- x + 1 / 2
  k <- dim(x)[3]
  or <- apply(x, 3, function(x) (x[1,1]*x[2,2])/(x[1,2]*x[2,1]))
  w <-  apply(x, 3, function(x) 1 / sum(1 / x))
  1 - pchisq(sum(w * (log(or) - weighted.mean(log(or), w)) ^ 2), k - 1)
}
woolf(UCBAdmissions)
## => p = 0.003, indicating that there is significant heterogeneity.
## (And hence the Mantel-Haenszel test cannot be used.)

## Agresti (2002), p. 287f and p. 297.
## Job Satisfaction example.
Satisfaction <-
    as.table(array(c(1, 2, 0, 0, 3, 3, 1, 2,
                     11, 17, 8, 4, 2, 3, 5, 2,
                     1, 0, 0, 0, 1, 3, 0, 1,
                     2, 5, 7, 9, 1, 1, 3, 6),
                   dim = c(4, 4, 2),
                   dimnames =
                   list(Income =
                        c("<5000", "5000-15000",
                          "15000-25000", ">25000"),
                        "Job Satisfaction" =
                        c("V_D", "L_S", "M_S", "V_S"),
                        Gender = c("Female", "Male"))))
## (Satisfaction categories abbreviated for convenience.)
ftable(. ~ Gender + Income, Satisfaction)
## Table 7.8 in Agresti (2002), p. 288.
mantelhaen.test(Satisfaction)
## See Table 7.12 in Agresti (2002), p. 297.
}
\keyword{htest}
