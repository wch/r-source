% File src/library/stats/man/t.test.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
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

\method{t.test}{formula}(formula, data, subset, na.action = na.pass, \dots)
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
    variance is used to estimate the variance otherwise the \I{Welch}
    (or \I{Satterthwaite}) approximation to the degrees of freedom is used.}
  \item{conf.level}{confidence level of the interval.}
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs}
    is a numeric variable giving the data values and \code{rhs} either 
    \code{1} for a one-sample or paired test or a factor
    with two levels giving the corresponding groups. If \code{lhs} is of 
    class \code{"\link{Pair}"} and \code{rhs} is \code{1}, a paired test
    is done, see Examples.}
  \item{data}{an optional matrix or data frame (or similar: see
    \code{\link{model.frame}}) containing the variables in the
    formula \code{formula}.  By default the variables are taken from
    \code{environment(formula)}.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used.}
  \item{na.action}{a function which indicates what should happen when
    the data contain \code{\link{NA}}s.}
  \item{\dots}{further arguments to be passed to or from methods.
    For the \code{formula} method, this includes arguments of the
    default method, but not \code{paired}.}
}
\details{
  \code{alternative = "greater"} is the alternative that \code{x} has a
  larger mean than \code{y}. For the one-sample case: that the mean is positive.

  If \code{paired} is \code{TRUE} then both \code{x} and \code{y} must
  be specified and they must be the same length.  Missing values are
  silently removed (in pairs if \code{paired} is \code{TRUE}).  If
  \code{var.equal} is \code{TRUE} then the pooled estimate of the
  variance is used.  By default, if \code{var.equal} is \code{FALSE}
  then the variance is estimated separately for both groups and the
  \I{Welch} modification to the degrees of freedom is used.

  If the input data are effectively constant (compared to the larger of the
  two means) an error is generated.

  If the data contain infinite values, \code{t.test()} no longer errors and
  returns a still not very useful result.  Note that
  \code{\link{wilcox.test}()} is \emph{robust} against outliers and hence
  deals more usefully with such \code{Inf} values in \code{x} or \code{y}.
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
  \item{stderr}{the standard error of the mean (difference), used as
    denominator in the t-statistic formula.}
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
## Two-sample t-test
t.test(1:10, y = c(7:20))      # P = .00001855
t.test(1:10, y = c(7:20, 200)) # P = .1245    -- NOT significant anymore

## Traditional interface
with(mtcars, t.test(mpg[am == 0], mpg[am == 1]))

## Formula interface
t.test(mpg ~ am, data = mtcars)

## One-sample t-test
## Traditional interface
t.test(sleep$extra)

## Formula interface
t.test(extra ~ 1, data = sleep)

## Paired t-test
## The sleep data is actually paired, so could have been in wide format:
sleep2 <- reshape(sleep, direction = "wide",
                  idvar = "ID", timevar = "group")

## Traditional interface
t.test(sleep2$extra.1, sleep2$extra.2, paired = TRUE)

## Formula interface
t.test(Pair(extra.1, extra.2) ~ 1, data = sleep2)
}
\keyword{htest}
