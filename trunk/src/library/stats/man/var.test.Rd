% File src/library/stats/man/var.test.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{var.test}
\alias{var.test}
\alias{var.test.default}
\alias{var.test.formula}
\title{F Test to Compare Two Variances}
\description{
  Performs an F test to compare the variances of two samples from normal
  populations.
}
\usage{
var.test(x, \dots)

\method{var.test}{default}(x, y, ratio = 1,
         alternative = c("two.sided", "less", "greater"),
         conf.level = 0.95, \dots)

\method{var.test}{formula}(formula, data, subset, na.action, \dots)
}
\arguments{
  \item{x, y}{numeric vectors of data values, or fitted linear model
    objects (inheriting from class \code{"lm"}).}
  \item{ratio}{the hypothesized ratio of the population variances of
    \code{x} and \code{y}.}
  \item{alternative}{a character string specifying the alternative
    hypothesis, must be one of \code{"two.sided"} (default),
    \code{"greater"} or \code{"less"}.  You can specify just the initial
    letter.}
  \item{conf.level}{confidence level for the returned confidence
    interval.}
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs}
    is a numeric variable giving the data values and \code{rhs} a factor
    with two levels giving the corresponding groups.}
  \item{data}{an optional matrix or data frame (or similar: see
    \code{\link{model.frame}}) containing the variables in the
    formula \code{formula}.  By default the variables are taken from
    \code{environment(formula)}.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used.}
  \item{na.action}{a function which indicates what should happen when
    the data contain \code{NA}s.  Defaults to
    \code{getOption("na.action")}.}
  \item{\dots}{further arguments to be passed to or from methods.}
}
\details{
  The null hypothesis is that the ratio of the variances of the
  populations from which \code{x} and \code{y} were drawn, or in the
  data to which the linear models \code{x} and \code{y} were fitted, is
  equal to \code{ratio}.
}
\value{
  A list with class \code{"htest"} containing the following components:
  \item{statistic}{the value of the F test statistic.}
  \item{parameter}{the degrees of the freedom of the F distribution of
    the test statistic.}
  \item{p.value}{the p-value of the test.}
  \item{conf.int}{a confidence interval for the ratio of the population
    variances.}
  \item{estimate}{the ratio of the sample variances of \code{x} and
    \code{y}.}
  \item{null.value}{the ratio of population variances under the null.}
  \item{alternative}{a character string describing the alternative
    hypothesis.}
  \item{method}{the character string
    \code{"F test to compare two variances"}.}
  \item{data.name}{a character string giving the names of the data.}
}
\seealso{
  \code{\link{bartlett.test}} for testing homogeneity of variances in
  more than two samples from normal distributions;
  \code{\link{ansari.test}} and \code{\link{mood.test}} for two rank
  based (nonparametric) two-sample tests for difference in scale.
}
\examples{
x <- rnorm(50, mean = 0, sd = 2)
y <- rnorm(30, mean = 1, sd = 1)
var.test(x, y)                  # Do x and y have the same variance?
var.test(lm(x ~ 1), lm(y ~ 1))  # The same.
}
\keyword{htest}
