% File src/library/stats/man/wilcox.test.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{wilcox.test}
\title{Wilcoxon Rank Sum and Signed Rank Tests}
\alias{wilcox.test}
\alias{wilcox.test.default}
\alias{wilcox.test.formula}
\concept{Mann-Whitney Test}
\description{
  Performs one- and two-sample Wilcoxon tests on vectors of data; the
  latter is also known as \sQuote{Mann-Whitney} test.
}
\usage{
wilcox.test(x, \dots)

\method{wilcox.test}{default}(x, y = NULL,
            alternative = c("two.sided", "less", "greater"),
            mu = 0, paired = FALSE, exact = NULL, correct = TRUE,
            conf.int = FALSE, conf.level = 0.95, \dots)

\method{wilcox.test}{formula}(formula, data, subset, na.action, \dots)
}
\arguments{
  \item{x}{numeric vector of data values.  Non-finite (e.g., infinite or
    missing) values will be omitted.}
  \item{y}{an optional numeric vector of data values: as with \code{x}
    non-finite values will be omitted.}
  \item{alternative}{a character string specifying the alternative
    hypothesis, must be one of \code{"two.sided"} (default),
    \code{"greater"} or \code{"less"}.  You can specify just the initial
    letter.}
  \item{mu}{a number specifying an optional parameter used to form the
    null hypothesis.  See \sQuote{Details}.}
  \item{paired}{a logical indicating whether you want a paired test.}
  \item{exact}{a logical indicating whether an exact p-value
    should be computed.}
  \item{correct}{a logical indicating whether to apply continuity
    correction in the normal approximation for the p-value.}
  \item{conf.int}{a logical indicating whether a confidence interval
    should be computed.}
  \item{conf.level}{confidence level of the interval.}
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs}
    is a numeric variable giving the data values and \code{rhs} either 
    \code{1} for a one-sample or paired test or a factor
    with two levels giving the corresponding groups. If \code{lhs} is of 
    class \code{"Pair"} and \code{rhs} is \code{1}, a paired test is done}
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

  If only \code{x} is given, or if both \code{x} and \code{y} are given
  and \code{paired} is \code{TRUE}, a Wilcoxon signed rank test of the
  null that the distribution of \code{x} (in the one sample case) or of
  \code{x - y} (in the paired two sample case) is symmetric about
  \code{mu} is performed.

  Otherwise, if both \code{x} and \code{y} are given and \code{paired}
  is \code{FALSE}, a Wilcoxon rank sum test (equivalent to the
  Mann-Whitney test: see the Note) is carried out.  In this case, the
  null hypothesis is that the distributions of \code{x} and \code{y}
  differ by a location shift of \code{mu} and the alternative is that
  they differ by some other location shift (and the one-sided
  alternative \code{"greater"} is that \code{x} is shifted to the right
  of \code{y}).

  By default (if \code{exact} is not specified), an exact p-value
  is computed if the samples contain less than 50 finite values and
  there are no ties.  Otherwise, a normal approximation is used.

  Optionally (if argument \code{conf.int} is true), a nonparametric
  confidence interval and an estimator for the pseudomedian (one-sample
  case) or for the difference of the location parameters \code{x-y} is
  computed.  (The pseudomedian of a distribution \eqn{F} is the median
  of the distribution of \eqn{(u+v)/2}, where \eqn{u} and \eqn{v} are
  independent, each with distribution \eqn{F}.  If \eqn{F} is symmetric,
  then the pseudomedian and median coincide.  See Hollander & Wolfe
  (1973), page 34.)  Note that in the two-sample case the estimator for
  the difference in location parameters does \bold{not} estimate the
  difference in medians (a common misconception) but rather the median
  of the difference between a sample from \code{x} and a sample from
  \code{y}.

  If exact p-values are available, an exact confidence interval is
  obtained by the algorithm described in Bauer (1972), and the
  Hodges-Lehmann estimator is employed.  Otherwise, the returned
  confidence interval and point estimate are based on normal
  approximations.  These are continuity-corrected for the interval but
  \emph{not} the estimate (as the correction depends on the
  \code{alternative}).

  With small samples it may not be possible to achieve very high
  confidence interval coverages. If this happens a warning will be given
  and an interval with lower coverage will be substituted.

  When \code{x} (and \code{y} if applicable) are valid, the function now
  always returns, also in the \code{conf.int = TRUE} case when a
  confidence interval cannot be computed, in which case the interval
  boundaries and sometimes the \code{estimate} now contain
  \code{\link{NaN}}.
}
\value{
  A list with class \code{"htest"} containing the following components:
  \item{statistic}{the value of the test statistic with a name
    describing it.}
  \item{parameter}{the parameter(s) for the exact distribution of the
    test statistic.}
  \item{p.value}{the p-value for the test.}
  \item{null.value}{the location parameter \code{mu}.}
  \item{alternative}{a character string describing the alternative
    hypothesis.}
  \item{method}{the type of test applied.}
  \item{data.name}{a character string giving the names of the data.}
  \item{conf.int}{a confidence interval for the location parameter.
    (Only present if argument \code{conf.int = TRUE}.)}
  \item{estimate}{an estimate of the location parameter.
    (Only present if argument \code{conf.int = TRUE}.)}
}
\note{
  The literature is not unanimous about the definitions of the Wilcoxon
  rank sum and Mann-Whitney tests.  The two most common definitions
  correspond to the sum of the ranks of the first sample with the
  minimum value subtracted or not: \R subtracts and S-PLUS does not,
  giving a value which is larger by \eqn{m(m+1)/2} for a first sample
  of size \eqn{m}.  (It seems Wilcoxon's original paper used the
  unadjusted sum of the ranks but subsequent tables subtracted the
  minimum.)

  \R's value can also be computed as the number of all pairs
  \code{(x[i], y[j])} for which \code{y[j]} is not greater than
  \code{x[i]}, the most common definition of the Mann-Whitney test.
}
\section{Warning}{
  This function can use large amounts of memory and stack (and even
  crash \R if the stack limit is exceeded) if \code{exact = TRUE} and
  one sample is large (several thousands or more).
}
\references{
  David F. Bauer (1972).
  Constructing confidence sets using rank statistics.
  \emph{Journal of the American Statistical Association}
  \bold{67}, 687--690.
  \doi{10.1080/01621459.1972.10481279}.

  Myles Hollander and Douglas A. Wolfe (1973).
  \emph{Nonparametric Statistical Methods}.
  New York: John Wiley & Sons.
  Pages 27--33 (one-sample), 68--75 (two-sample).\cr
  Or second edition (1999).
}
\seealso{
  \code{\link{psignrank}}, \code{\link{pwilcox}}.

  \code{\link[coin:LocationTests]{wilcox_test}} in package
  \CRANpkg{coin} for exact, asymptotic and Monte Carlo
  \emph{conditional} p-values, including in the presence of ties.

  \code{\link{kruskal.test}} for testing homogeneity in location
  parameters in the case of two or more samples;
  \code{\link{t.test}} for an alternative under normality
  assumptions [or large samples]
}
\examples{
require(graphics)
## One-sample test.
## Hollander & Wolfe (1973), 29f.
## Hamilton depression scale factor measurements in 9 patients with
##  mixed anxiety and depression, taken at the first (x) and second
##  (y) visit after initiation of a therapy (administration of a
##  tranquilizer).
x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
wilcox.test(x, y, paired = TRUE, alternative = "greater")
wilcox.test(y - x, alternative = "less")    # The same.
wilcox.test(y - x, alternative = "less",
            exact = FALSE, correct = FALSE) # H&W large sample
                                            # approximation

## Formula interface to one-sample and paired tests

depression <- data.frame(first = x, second = y, change = y - x)
wilcox.test(change ~ 1, data = depression)
wilcox.test(Pair(first, second) ~ 1, data = depression)

## Two-sample test.
## Hollander & Wolfe (1973), 69f.
## Permeability constants of the human chorioamnion (a placental
##  membrane) at term (x) and between 12 to 26 weeks gestational
##  age (y).  The alternative of interest is greater permeability
##  of the human chorioamnion for the term pregnancy.
x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
y <- c(1.15, 0.88, 0.90, 0.74, 1.21)
wilcox.test(x, y, alternative = "g")        # greater
wilcox.test(x, y, alternative = "greater",
            exact = FALSE, correct = FALSE) # H&W large sample
                                            # approximation

wilcox.test(rnorm(10), rnorm(10, 2), conf.int = TRUE)

## Formula interface.
boxplot(Ozone ~ Month, data = airquality)
wilcox.test(Ozone ~ Month, data = airquality,
            subset = Month \%in\% c(5, 8))

}


\keyword{htest}
