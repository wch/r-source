% File src/library/stats/man/ks.test.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2023 R Core Team
% Distributed under GPL 2 or later

\name{ks.test}
\alias{ks.test}
\alias{ks.test.default}
\alias{ks.test.formula}
\encoding{UTF-8}
\title{Kolmogorov-Smirnov Tests}
\description{
  Perform a one- or two-sample Kolmogorov-Smirnov test.
}
\usage{
ks.test(x, \dots)
\method{ks.test}{default}(x, y, \dots,
        alternative = c("two.sided", "less", "greater"),
        exact = NULL, simulate.p.value = FALSE, B = 2000)
\method{ks.test}{formula}(formula, data, subset, na.action, \dots)
}
\arguments{
  \item{x}{a numeric vector of data values.}
  \item{y}{either a numeric vector of data values, or a character string
    naming a cumulative distribution function or an actual cumulative
    distribution function such as \code{pnorm}.  Only continuous CDFs
    are valid.}
  \item{\dots}{for the default method, parameters of the distribution
    specified (as a character string) by \code{y}.  Otherwise, further
    arguments to be passed to or from methods.}
  \item{alternative}{indicates the alternative hypothesis and must be
    one of \code{"two.sided"} (default), \code{"less"}, or
    \code{"greater"}.  You can specify just the initial letter of the
    value, but the argument name must be given in full.
    See \sQuote{Details} for the meanings of the possible values.}
  \item{exact}{\code{NULL} or a logical indicating whether an exact
    p-value should be computed.  See \sQuote{Details} for the meaning of
    \code{NULL}.}
  \item{simulate.p.value}{a logical indicating whether to compute
    p-values by Monte Carlo simulation.  (Ignored for the one-sample
    test.)}
  \item{B}{an integer specifying the number of replicates used in the
    Monte Carlo test.}
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs}
    is a numeric variable giving the data values and \code{rhs} either
    \code{1} for a one-sample test or a factor with two levels giving
    the corresponding groups for a two-sample test.}
  \item{data}{an optional matrix or data frame (or similar: see
    \code{\link{model.frame}}) containing the variables in the
    formula \code{formula}.  By default the variables are taken from
    \code{environment(formula)}.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used.}
  \item{na.action}{a function which indicates what should happen when
    the data contain \code{NA}s.  Defaults to
    \code{getOption("na.action")}.}
}
\details{
  If \code{y} is numeric, a two-sample (Smirnov) test of the null
  hypothesis that \code{x} and \code{y} were drawn from the same
  distribution is performed.

  Alternatively, \code{y} can be a character string naming a continuous
  (cumulative) distribution function, or such a function.  In this case,
  a one-sample (Kolmogorov) test is carried out of the null that the
  distribution function which generated \code{x} is distribution
  \code{y} with parameters specified by \code{\dots}.
  The presence of ties always generates a warning in the one-sample case, as continuous
  distributions do not generate them.  If the ties arose from rounding
  the tests may be approximately valid, but even modest amounts of
  rounding can have a significant effect on the calculated statistic.

  Missing values are silently omitted from \code{x} and (in the
  two-sample case) \code{y}.

  The possible values \code{"two.sided"}, \code{"less"} and
  \code{"greater"} of \code{alternative} specify the null hypothesis
  that the true cumulative distribution function (CDF) of \code{x} is equal
  to, not less than or not greater than the hypothesized CDF (one-sample
  case) or the CDF of \code{y} (two-sample case), respectively.  The test
  compares the CDFs taking their maximal difference as test statistic,
  with the statistic in the \code{"greater"} alternative being
  \eqn{D^+ = \max_u [ F_x(u) - F_y(u) ]}{D^+ = max[F_x(u) - F_y(u)]}.
  Thus in the two-sample case \code{alternative = "greater"} includes
  distributions for which \code{x} is stochastically \emph{smaller} than
  \code{y} (the CDF of \code{x} lies above and hence to the left of that
  for \code{y}), in contrast to \code{\link{t.test}} or
  \code{\link{wilcox.test}}.

  Exact p-values are not available for the one-sample case in the
  presence of ties.
  If \code{exact = NULL} (the default), an
  exact p-value is computed if the sample size is less than 100 in the
  one-sample case \emph{and there are no ties}, and if the product of
  the sample sizes is less than 10000 in the two-sample case, with or
  without ties (using the algorithm described in
  \bibcite{Schröer and Trenkler (1995)}).
  Otherwise, the p-value is computed via Monte Carlo simulation in the
  two-sample case if \code{simulate.p.value} is \code{TRUE}, or else
  asymptotic distributions are used whose approximations may
  be inaccurate in small samples.  In the one-sample two-sided case,
  exact p-values are obtained as described in
  \bibcite{Marsaglia, Tsang & Wang (2003)}
  (but not using the optional approximation in the right tail, so
  this can be slow for small p-values).  The formula of
  \bibcite{Birnbaum & Tingey (1951)} is used for the one-sample
  one-sided case. 

  If a one-sample test is used, the parameters specified in
  \code{\dots} must be pre-specified and not estimated from the data.
  There is some more refined distribution theory for the KS test with
  estimated parameters (see \bibcite{Durbin, 1973}), but that is not implemented
  in \code{ks.test}.
}
\value{
  A list inheriting from classes \code{"ks.test"} and \code{"htest"}
  containing the following components:
  \item{statistic}{the value of the test statistic.}
  \item{p.value}{the p-value of the test.}
  \item{alternative}{a character string describing the alternative
    hypothesis.}
  \item{method}{a character string indicating what type of test was
    performed.}
  \item{data.name}{a character string giving the name(s) of the data.}
}
\source{
  The two-sided one-sample distribution comes \emph{via}
  Marsaglia, Tsang and Wang (2003).

  Exact distributions for the two-sample (Smirnov) test are computed
  by the algorithm proposed by Schröer (1991) and Schröer & Trenkler (1995)
  using numerical improvements along the lines of Viehmann (2021).
}
\references{
  Z. W. Birnbaum and Fred H. Tingey (1951).
  One-sided confidence contours for probability distribution functions.
  \emph{The Annals of Mathematical Statistics}, \bold{22}/4, 592--596.
  \doi{10.1214/aoms/1177729550}.

  William J. Conover (1971).
  \emph{Practical Nonparametric Statistics}.
  New York: John Wiley & Sons.
  Pages 295--301 (one-sample Kolmogorov test),
  309--314 (two-sample Smirnov test).

  Durbin, J. (1973).
  \emph{Distribution theory for tests based on the sample distribution
    function}.
  SIAM.

  W. Feller (1948).
  On the Kolmogorov-Smirnov limit theorems for empirical distributions.
  \emph{The Annals of Mathematical Statistics}, \bold{19}(2), 177--189.
  \doi{10.1214/aoms/1177730243}.

  George Marsaglia, Wai Wan Tsang and Jingbo Wang (2003).
  Evaluating Kolmogorov's distribution.
  \emph{Journal of Statistical Software}, \bold{8}/18.
  \doi{10.18637/jss.v008.i18}.

  Gunar Schröer (1991).
  Computergestützte statistische Inferenz am Beispiel der
  Kolmogorov-Smirnov Tests.
  Diplomarbeit Universität Osnabrück.

  Gunar Schröer and Dietrich Trenkler (1995).
  Exact and Randomization Distributions of Kolmogorov-Smirnov Tests for
  Two or Three Samples.
  \emph{Computational Statistics & Data Analysis}, \bold{20}(2),
  185--202.
  \doi{10.1016/0167-9473(94)00040-P}.

  Thomas Viehmann (2021).
  Numerically more stable computation of the p-values for the two-sample
  Kolmogorov-Smirnov test.
  \url{https://arxiv.org/abs/2102.08037}.
}
\seealso{
  \code{\link{psmirnov}}.

  \code{\link{shapiro.test}} which performs the \I{Shapiro}-\I{Wilk} test for
  normality.
}
\examples{
require("graphics")

x <- rnorm(50)
y <- runif(30)
# Do x and y come from the same distribution?
ks.test(x, y)
# Does x come from a shifted gamma distribution with shape 3 and rate 2?
ks.test(x+2, "pgamma", 3, 2) # two-sided, exact
ks.test(x+2, "pgamma", 3, 2, exact = FALSE)
ks.test(x+2, "pgamma", 3, 2, alternative = "gr")

# test if x is stochastically larger than x2
x2 <- rnorm(50, -1)
plot(ecdf(x), xlim = range(c(x, x2)))
plot(ecdf(x2), add = TRUE, lty = "dashed")
t.test(x, x2, alternative = "g")
wilcox.test(x, x2, alternative = "g")
ks.test(x, x2, alternative = "l")

# with ties, example from Schröer and Trenkler (1995)
# D = 3/7, p = 8/33 = 0.242424..
ks.test(c(1, 2, 2, 3, 3),
        c(1, 2, 3, 3, 4, 5, 6))# -> exact

# formula interface, see ?wilcox.test
ks.test(Ozone ~ Month, data = airquality,
        subset = Month \%in\% c(5, 8))
}
\keyword{htest}
