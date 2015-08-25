% File src/library/stats/man/t.test.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{t.test}
\alias{t.test}
\alias{t.test.default}
\alias{t.test.formula}
\title{Student's t-Test}
\description{
  Performs one and two sample t-tests on vectors of data.
}
\usage{
t.test(x, \dots)

\method{t.test}{default}(x, y = NULL,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95, \dots)

\method{t.test}{formula}(formula, data, subset, na.action, \dots)
}
\arguments{
  \item{x}{a (non-empty) numeric vector of data values.}
  \item{y}{an optional (non-empty) numeric vector of data values.}
  \item{alternative}{a character string specifying the alternative
    hypothesis, must be one of \code{"two.sided"} (default),
    \code{"greater"} or \code{"less"}.  You can specify just the initial
    letter.}
  \item{mu}{a number indicating the true value of the mean (or
    difference in means if you are performing a two sample test).}
  \item{paired}{a logical indicating whether you want a paired
    t-test.}
  \item{var.equal}{a logical variable indicating whether to treat the
    two variances as being equal. If \code{TRUE} then the pooled
    variance is used to estimate the variance otherwise the Welch
    (or Satterthwaite) approximation to the degrees of freedom is used.}
  \item{conf.level}{confidence level of the interval.}
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
  The formula interface is only applicable for the 2-sample tests.

  \code{alternative = "greater"} is the alternative that \code{x} has a
  larger mean than \code{y}.

  If \code{paired} is \code{TRUE} then both \code{x} and \code{y} must
  be specified and they must be the same length.  Missing values are
  silently removed (in pairs if \code{paired} is \code{TRUE}).  If
  \code{var.equal} is \code{TRUE} then the pooled estimate of the
  variance is used.  By default, if \code{var.equal} is \code{FALSE}
  then the variance is estimated separately for both groups and the
  Welch modification to the degrees of freedom is used.

  If the input data are effectively constant (compared to the larger of the
  two means) an error is generated.
}
\value{
  A list with class \code{"htest"} containing the following components:
  \item{statistic}{the value of the t-statistic.}
  \item{parameter}{the degrees of freedom for the t-statistic.}
  \item{p.value}{the p-value for the test.}
  \item{conf.int}{a confidence interval for the mean appropriate to the
    specified alternative hypothesis.}
  \item{estimate}{the estimated mean or difference in means depending on
    whether it was a one-sample test or a two-sample test.}
  \item{null.value}{the specified hypothesized value of the mean or mean
    difference depending on whether it was a one-sample test or a
    two-sample test.}
  \item{alternative}{a character string describing the alternative
    hypothesis.}
  \item{method}{a character string indicating what type of t-test was
    performed.}
  \item{data.name}{a character string giving the name(s) of the data.}
}
\seealso{
  \code{\link{prop.test}}
}
\examples{
require(graphics)

t.test(1:10, y = c(7:20))      # P = .00001855
t.test(1:10, y = c(7:20, 200)) # P = .1245    -- NOT significant anymore

## Classical example: Student's sleep data
plot(extra ~ group, data = sleep)
## Traditional interface
with(sleep, t.test(extra[group == 1], extra[group == 2]))
## Formula interface
t.test(extra ~ group, data = sleep)
}
\keyword{htest}
