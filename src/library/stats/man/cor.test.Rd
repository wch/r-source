% File src/library/stats/man/cor.test.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{cor.test}
\alias{cor.test}
\alias{cor.test.default}
\alias{cor.test.formula}
\concept{Kendall correlation coefficient}
\concept{Kendall's tau}
\concept{Pearson correlation coefficient}
\concept{Spearman correlation coefficient}
\concept{Spearman's rho}
\title{Test for Association/Correlation Between Paired Samples}
\description{
  Test for association between paired samples, using one of
  Pearson's product moment correlation coefficient,
  Kendall's \eqn{\tau}{tau} or Spearman's \eqn{\rho}{rho}.
}
\usage{
cor.test(x, \dots)

\method{cor.test}{default}(x, y,
         alternative = c("two.sided", "less", "greater"),
         method = c("pearson", "kendall", "spearman"),
         exact = NULL, conf.level = 0.95, continuity = FALSE, \dots)

\method{cor.test}{formula}(formula, data, subset, na.action, \dots)
}
\arguments{
  \item{x, y}{numeric vectors of data values.  \code{x} and \code{y}
    must have the same length.}
  \item{alternative}{indicates the alternative hypothesis and must be
    one of \code{"two.sided"}, \code{"greater"} or \code{"less"}.  You
    can specify just the initial letter.  \code{"greater"} corresponds
    to positive association, \code{"less"} to negative association.}
  \item{method}{a character string indicating which correlation
    coefficient is to be  used for the test.  One of \code{"pearson"},
    \code{"kendall"}, or \code{"spearman"}, can be abbreviated.}
  \item{exact}{a logical indicating whether an exact p-value should be
    computed.  Used for Kendall's \eqn{\tau}{tau} and
    Spearman's \eqn{\rho}{rho}.
    See \sQuote{Details} for the meaning of \code{NULL} (the default).}
  \item{conf.level}{confidence level for the returned confidence
    interval.  Currently only used for the Pearson product moment
    correlation coefficient if there are at least 4 complete pairs of
    observations.}
  \item{continuity}{logical: if true, a continuity correction is used
    for Kendall's \eqn{\tau}{tau} and Spearman's \eqn{\rho}{rho} when
    not computed exactly.}
  \item{formula}{a formula of the form \code{~ u + v}, where each of
    \code{u} and \code{v} are numeric variables giving the data values
    for one sample.  The samples must be of the same length.}
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
\value{
  A list with class \code{"htest"} containing the following components:
  \item{statistic}{the value of the test statistic.}
  \item{parameter}{the degrees of freedom of the test statistic in the
    case that it follows a t distribution.}
  \item{p.value}{the p-value of the test.}
  \item{estimate}{the estimated measure of association, with name
    \code{"cor"}, \code{"tau"}, or \code{"rho"} corresponding
    to the method employed.}
  \item{null.value}{the value of the association measure under the
    null hypothesis, always \code{0}.}
  \item{alternative}{a character string describing the alternative
    hypothesis.}
  \item{method}{a character string indicating how the association was
    measured.}
  \item{data.name}{a character string giving the names of the data.}
  \item{conf.int}{a confidence interval for the measure of association.
    Currently only given for Pearson's product moment correlation
    coefficient in case of at least 4 complete pairs of observations.}
}
\details{
  The three methods each estimate the association between paired samples
  and compute a test of the value being zero.  They use different
  measures of association, all in the range \eqn{[-1, 1]} with \eqn{0}
  indicating no association.  These are sometimes referred to as tests
  of no \emph{correlation}, but that term is often confined to the
  default method.

  If \code{method} is \code{"pearson"}, the test statistic is based on
  Pearson's product moment correlation coefficient \code{cor(x, y)} and
  follows a t distribution with \code{length(x)-2} degrees of freedom
  if the samples follow independent normal distributions.  If there are
  at least 4 complete pairs of observation, an asymptotic confidence
  interval is given based on Fisher's Z transform.

  If \code{method} is \code{"kendall"} or \code{"spearman"}, Kendall's
  \eqn{\tau}{tau} or Spearman's \eqn{\rho}{rho} statistic is used to
  estimate a rank-based measure of association.  These tests may be used
  if the data do not necessarily come from a bivariate normal
  distribution.

  For Kendall's test, by default (if \code{exact} is NULL), an exact
  p-value is computed if there are less than 50 paired samples containing
  finite values and there are no ties.  Otherwise, the test statistic is
  the estimate scaled to zero mean and unit variance, and is approximately
  normally distributed.

  For Spearman's test, p-values are computed using algorithm AS 89 for
  \eqn{n < 1290} and \code{exact = TRUE}, otherwise via the asymptotic
  \eqn{t} approximation.  Note that these are \sQuote{exact} for \eqn{n
  < 10}, and use an Edgeworth series approximation for larger sample
  sizes (the cutoff has been changed from the original paper).
}
\references{
  D. J. Best & D. E. Roberts (1975).
  Algorithm AS 89: The Upper Tail Probabilities of Spearman's \eqn{\rho}{rho}.
  \emph{Applied Statistics}, \bold{24}, 377--379.
  \doi{10.2307/2347111}.

  Myles Hollander & Douglas A. Wolfe (1973),
  \emph{Nonparametric Statistical Methods.}
  New York: John Wiley & Sons.
  Pages 185--194 (Kendall and Spearman tests).
}

\seealso{
  \code{\link[Kendall:Kendall]{Kendall}} in package \CRANpkg{Kendall}.

  \code{\link[SuppDists:Kendall]{pKendall}} and
  \code{\link[SuppDists:Spearman]{pSpearman}} in package
  \CRANpkg{SuppDists},
  \code{\link[pspearman:spearman.test]{spearman.test}} in package
  \CRANpkg{pspearman},
  which supply different (and often more accurate) approximations.
}

\examples{
## Hollander & Wolfe (1973), p. 187f.
## Assessment of tuna quality.  We compare the Hunter L measure of
##  lightness to the averages of consumer panel scores (recoded as
##  integer values from 1 to 6 and averaged over 80 such values) in
##  9 lots of canned tuna.

x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)

##  The alternative hypothesis of interest is that the
##  Hunter L value is positively associated with the panel score.

cor.test(x, y, method = "kendall", alternative = "greater")
## => p=0.05972

cor.test(x, y, method = "kendall", alternative = "greater",
         exact = FALSE) # using large sample approximation
## => p=0.04765

## Compare this to
cor.test(x, y, method = "spearm", alternative = "g")
cor.test(x, y,                    alternative = "g")

## Formula interface.
require(graphics)
pairs(USJudgeRatings)
cor.test(~ CONT + INTG, data = USJudgeRatings)
}
\keyword{htest}
