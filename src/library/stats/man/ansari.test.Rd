% File src/library/stats/man/ansari.test.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{ansari.test}
\alias{ansari.test}
\alias{ansari.test.default}
\alias{ansari.test.formula}
\title{Ansari-Bradley Test}
\description{
  Performs the Ansari-Bradley two-sample test for a difference in scale
  parameters.
}
\usage{
ansari.test(x, \dots)

\method{ansari.test}{default}(x, y,
            alternative = c("two.sided", "less", "greater"),
            exact = NULL, conf.int = FALSE, conf.level = 0.95,
            \dots)

\method{ansari.test}{formula}(formula, data, subset, na.action, \dots)
}
\arguments{
  \item{x}{numeric vector of data values.}
  \item{y}{numeric vector of data values.}
  \item{alternative}{indicates the alternative hypothesis and must be
    one of \code{"two.sided"}, \code{"greater"} or \code{"less"}.  You
    can specify just the initial letter.}
  \item{exact}{a logical indicating whether an exact p-value
    should be computed.}
  \item{conf.int}{a logical,indicating whether a confidence interval
    should be computed.}
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
  Suppose that \code{x} and \code{y} are independent samples from
  distributions with densities \eqn{f((t-m)/s)/s} and \eqn{f(t-m)},
  respectively, where \eqn{m} is an unknown nuisance parameter and
  \eqn{s}, the ratio of scales, is the parameter of interest.  The
  Ansari-Bradley test is used for testing the null that \eqn{s} equals
  1, the two-sided alternative being that \eqn{s \ne 1}{s != 1} (the
  distributions differ only in variance), and the one-sided alternatives
  being \eqn{s > 1} (the distribution underlying \code{x} has a larger
  variance, \code{"greater"}) or \eqn{s < 1} (\code{"less"}).

  By default (if \code{exact} is not specified), an exact p-value
  is computed if both samples contain less than 50 finite values and
  there are no ties.  Otherwise, a normal approximation is used.

  Optionally, a nonparametric confidence interval and an estimator for
  \eqn{s} are computed.  If exact p-values are available, an exact
  confidence interval is obtained by the algorithm described in Bauer
  (1972), and the Hodges-Lehmann estimator is employed.  Otherwise, the
  returned confidence interval and point estimate are based on normal
  approximations.

  Note that mid-ranks are used in the case of ties rather than average
  scores as employed in Hollander & Wolfe (1973).  See, e.g., Hajek,
  Sidak and Sen (1999), pages 131ff, for more information.
}
\value{
  A list with class \code{"htest"} containing the following components:
  \item{statistic}{the value of the Ansari-Bradley test statistic.}
  \item{p.value}{the p-value of the test.}
  \item{null.value}{the ratio of scales \eqn{s} under the null, 1.}
  \item{alternative}{a character string describing the alternative
    hypothesis.}
  \item{method}{the string \code{"Ansari-Bradley test"}.}
  \item{data.name}{a character string giving the names of the data.}
  \item{conf.int}{a confidence interval for the scale parameter.
    (Only present if argument \code{conf.int = TRUE}.)}
  \item{estimate}{an estimate of the ratio of scales.
    (Only present if argument \code{conf.int = TRUE}.)}
}
\note{
  To compare results of the Ansari-Bradley test to those of the F test
  to compare two variances (under the assumption of normality), observe
  that \eqn{s} is the ratio of scales and hence \eqn{s^2} is the ratio
  of variances (provided they exist), whereas for the F test the ratio
  of variances itself is the parameter of interest.  In particular,
  confidence intervals are for \eqn{s} in the Ansari-Bradley test but
  for \eqn{s^2} in the F test.
}
\references{
  David F. Bauer (1972),
  Constructing confidence sets using rank statistics.
  \emph{Journal of the American Statistical Association}
  \bold{67}, 687--690.

  Jaroslav Hajek, Zbynek Sidak and Pranab K. Sen (1999),
  \emph{Theory of Rank Tests}.
  San Diego, London: Academic Press.

  Myles Hollander and Douglas A. Wolfe (1973),
  \emph{Nonparametric Statistical Methods.}
  New York: John Wiley & Sons.
  Pages 83--92.
}
\seealso{
  \code{\link{fligner.test}} for a rank-based (nonparametric)
  \eqn{k}-sample test for homogeneity of variances;
  \code{\link{mood.test}} for another rank-based two-sample test for a
  difference in scale parameters;
  \code{\link{var.test}} and \code{\link{bartlett.test}} for parametric
  tests for the homogeneity in variance.

  \code{\link[coin:ScaleTests]{ansari_test}} in package \CRANpkg{coin}
  for exact and approximate \emph{conditional} p-values for the
  Ansari-Bradley test, as well as different methods for handling ties.
}
\examples{
## Hollander & Wolfe (1973, p. 86f):
## Serum iron determination using Hyland control sera
ramsay <- c(111, 107, 100, 99, 102, 106, 109, 108, 104, 99,
            101, 96, 97, 102, 107, 113, 116, 113, 110, 98)
jung.parekh <- c(107, 108, 106, 98, 105, 103, 110, 105, 104,
            100, 96, 108, 103, 104, 114, 114, 113, 108, 106, 99)
ansari.test(ramsay, jung.parekh)

ansari.test(rnorm(10), rnorm(10, 0, 2), conf.int = TRUE)

## try more points - failed in 2.4.1
ansari.test(rnorm(100), rnorm(100, 0, 2), conf.int = TRUE)
}
\keyword{htest}
