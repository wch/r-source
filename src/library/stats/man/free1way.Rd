% File src/library/stats/man/free1way.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2026 R Core Team
% Distributed under GPL 2 or later

\name{free1way}
\alias{free1way}
\alias{free1way.formula}
\alias{free1way.table}
\alias{free1way.numeric}
\alias{free1way.factor}
\alias{print.free1way}
\alias{coef.free1way}
\alias{vcov.free1way}
\alias{logLik.free1way}
\alias{summary.free1way}
\alias{confint.free1way}
\title{
Distribution-free Inference in a Stratified One-Way Layout
}
\description{
Estimation, tests, and confidence intervals for parameters in the
distribution-free stratified K-sample one-way layout for binary, ordinal,
numeric and potentially independently right-censored outcomes, including
semiparametrically efficient score tests against Lehmann, odds ratio, or
hazard ratio alternatives with corresponding confidence intervals.
}
\usage{
free1way(y, ...)
\S3method{free1way}{formula}(formula, data, weights, subset, na.action = na.pass,
         event = NULL, ...)
\S3method{free1way}{numeric}(y, groups, blocks = NULL, event = NULL, weights = NULL, nbins = 0, 
         varnames = NULL, ...)
\S3method{free1way}{factor}(y, groups, blocks = NULL, event = NULL, weights = NULL, 
         varnames = NULL, ...)
\S3method{free1way}{table}(y, link = c("logit", "probit", "cloglog", "loglog"), 
         mu = 0, B = 0, exact = FALSE, ...)
\S3method{summary}{free1way}(object, test, alternative = c("two.sided", "less", "greater"), 
        tol = .Machine$double.eps, ...)
\S3method{coef}{free1way}(object, what = c("shift", "PI", "AUC", "OVL"), ...)
\S3method{confint}{free1way}(object, parm, level = .95, 
        test = c("Permutation", "Wald", "LRT", "Rao"), 
        what = c("shift", "PI", "AUC", "OVL"), ...)
\S3method{vcov}{free1way}(object, ...)
\S3method{logLik}{free1way}(object, ...)
}
\arguments{
  \item{y}{a binary factor, an ordered factor, a numeric vector, or a
    \code{\link[survival]{Surv}} object (right-censoring only) containing
    the response values or a \code{\link{table}} with the response categories in the first
    dimension, the groups in the second dimension, and, optionally, blocks and
    event indicators as third and fourth dimensions.}
  \item{nbins}{an optional integer defining the number of intervals to
    divide the range of a numeric response \code{y} into. The default is to
    cut the observations at breaks given by the uniquely observed values (\code{nbins = 0}). In
    the presence of right-censoring, uniquely observed event times define
    the breaks. For large sample sizes with many unique observations,
    limiting the number of bins to less than 100, say, dramatically reduces
    computation time while producing almost the same results as the default.
    The latter option is only available in the absence of right-censoring.}
  \item{groups}{a grouping factor with at least two non-empty levels.}
  \item{blocks}{a stratification factor, optional.}
  \item{event}{a logical vector representing events (\code{TRUE}) and
    independently right-censored observations (\code{FALSE}), optional.
    Right-censoring can be specified by either using \code{Surv} as a
    response in a formula or by a logical \code{event} argument (where
    \code{FALSE} indicates a right-censored observation).}
  \item{formula}{a formula of the form \code{y ~ groups | blocks} where \code{y} gives 
    the sample outcome values (binary, ordered, numeric, or \code{\link[survival]{Surv}}) 
    and \code{groups} the corresponding groups.
    In stratified designs, a \code{blocks} term specifies the strata.}
  \item{data}{an optional data frame (or similar: see
    \code{\link{model.frame}}) containing the variables in the
    formula \code{formula}.  By default the variables are taken from
    \code{environment(formula)}.}
  \item{weights}{an optional vector of weights to be used in the fitting
    process.  Should be \code{NULL} or a numeric vector.  If non-NULL,
    the weighted log-likelihood is maximised.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used.}
  \item{na.action}{a function which indicates what should happen when
    the data contain \code{NA}s.  Defaults to
    \code{getOption("na.action")}.}
  \item{varnames}{a character vector giving the names of the response,
    grouping, and stratifying variables.}
  \item{link}{a character defining a link function and thus the model and
    parameter interpretation. See \sQuote{Details}.}
  \item{mu}{a vector specifying optional parameters used to form the
    null hypothesis.  See \sQuote{Details}.}	
  \item{B}{an integer specifying the number of replicates used in the
    permutation test. The default (\code{B = 0}) performs an asymptotic
    permutation test, \code{B > 0} uses \code{B} Monte Carlo replications to
    approximate the permutation distribution.}
  \item{exact}{a logical requesting the exact permutation distribution
    to be computed. Only available for unstratified two-sample proportional odds
    models.}
  \item{object}{an object of class \code{free1way} as returned by
    \code{free1way}.}
  \item{test}{a character vector defining the global test procedure for all
    parameters: \code{"Permutation"} performs a conditional permutation score test
    under the randomisation model, \code{"Wald"} performs a Wald test,
    \code{"LRT"} a likelihood ratio, and \code{"Rao"} a Rao score test under the
    population model.}
  \item{alternative}{a character string specifying the alternative
    hypothesis, must be one of \code{"two.sided"} (default),
    \code{"greater"} or \code{"less"}.}
  \item{what}{a character defining a monotone transformation of the
    shift parameters: probabilistic indices (\code{"PI"}), area under the
    curves (\code{"AUC"}), or overlap coefficients (\code{"OVL"}). The
    default is to return parameters on the original shift scale.}
  \item{tol}{a positive numeric tolerance.}
  \item{parm}{a specification of which parameters are to be given
          confidence intervals, either a vector of numbers or a vector
          of names.  If missing, all parameters are considered.}
  \item{level}{the confidence level required.}
  \item{\dots}{arguments passed to the \code{table} method for 
          \code{free1way}.}
}
\details{

The distribution function \eqn{F_1} of the response in the control group
(defined by the first factor level of the grouping factor \code{groups}) is
compared to the distribution functions in the remaining groups \eqn{F_k} for
\eqn{k = 2, \dots, K}. No assumptions are made regarding the form of \eqn{F_1},
all inference procedures are thus distribution-free. However, a
semiparametric model assumes that each distribution function \eqn{F_k} only
depends on \eqn{F_1} and a scalar parameter \eqn{\delta_k}. The link
function \eqn{Q} defines the model
\deqn{F_k(y) = Q^{-1}(Q(F_1(y)) - \delta_k)}
such that positive values of the parameter \eqn{\delta_k} correspond to
stochasically larger response values in group \eqn{k} when compared to the
control group. Here, \eqn{\mu_k} defines the null hypothesis \eqn{H_0^k: \delta_k
- \mu_k = 0, k = 2, \dots, K} (or the one-sided equivalents thereof); 
the parameter \code{mu} can be a vector of length \eqn{K - 1} or a
scalar value to be recycled to this length.

The argument \code{link} is used to define the link function and thus
specific models: Log-odds ratio alternatives are based on the logit link
(\code{link = "logit"}, with \eqn{Q} being the quantile function of the
standard logistic distribution)
\deqn{\log\left(\frac{F_k(y)}{1 - F_k(y)}\right) = \log\left(\frac{F_1(y)}{1 - F_1(y)}\right) - \delta_k,}
log-hazard ratio alternatives on the complementary log-log link (\code{link =
"cloglog"}, with \eqn{Q} being the quantile function of the Gompertz
minimum extreme value distribution)
\deqn{1 - F_k(y) = (1 - F_1(y))^{\exp(- \delta_k)},} Lehmann (or reverse
time log-hazard ratio) alternatives on
the log-log link (\code{link = "loglog"}, with \eqn{Q} being the quantile
function of the Gumbel maximum extreme value distribution)
\deqn{F_k(y) = F_1(y)^{\exp(- \delta_k)},} and a shift alternative on a
latent normal scale (similar in spirit to Cohen's \eqn{d}) on the probit link (\code{link = "probit"}, with \eqn{Q}
being the quantile function of the standard normal distribution)
\deqn{\Phi^{-1}(F_1(Y_k)) \sim N(\delta_k, 1)}
for random variables \eqn{Y_k \sim F_k}.
If strata (independent blocks) are present, the distribution functions may be stratum-specific
but the shift parameters are assumed to be constant across strata, such that
the above model holds for each stratum.

The control distribution function \eqn{F_1} is treated as a nuisance
parameter and the shift parameters \eqn{\delta_2, \dots, \delta_K} are
estimated by empirical maximum-likelihood estimation, the maximised
log-empirical likelihood is available via \code{logLik}. Parameter estimates and the
inverse observed Fisher information can be inspected via \code{coef} and
\code{vcov}.

Several global test procedures for the null hypothesis that all distribution
functions are identical (and thus \eqn{\delta_2 = \dots = \delta_K = 0} when
\code{mu = 0}, otherwise the global null of all \eqn{H_0^k} holding
simultaneously is tested) can be specified by the \code{test} argument. 
Parameter-specific confidence intervals are obtained via the inversion 
of a specific test procedure. 

Parameter interpretation might be easier on transformed scales, such as
odds-ratios or hazard ratios. In addition, the treatment effects in such
semiparametric models can be transformed into probabilistic indices
(\code{what = "PI"}, being equivalent to the area under the curve
\code{"AUC"}) or overlap coefficients (\code{what = "OVL"}), under the
assumption that the model is correct. These model-based estimators must not
be confused with assumption-free estimators, such as for example the
Wilcoxon-Mann-Whitney-U statistic for the AUC
\bibcitep{R:Fay+Malinovsky:2018}. Confidence intervals for the
model-based measures are best obtained by inverting permutation score,
likelihood ratio, or Rao score tests \bibcitep{R:Sewak+Hothorn:2023},
exploiting their invariance with respect to monotone transformations.

Assuming one of the semiparametric models, the parameter estimates are
semiparametrically efficient and the corresponding score tests and
confidence intervals are locally most powerful 
\bibcitep{Chapter 15.5 in |R:Van_der_Vaart:1998|}. An introduction to
proportional-odds models in this class is available in
\bibcitet{R:Harrell:2015}.

\code{free1way} allows several classical tests to be performed and enhanced
with corresonding parameter estimates and a variety of inference
procedures under the randomisation and population model. 
The two-sample Wilcoxon rank sum test (implemented in
\code{\link{wilcox.test}}) is obtained with the default \code{link =
"logit"} as is the \eqn{K}-sample Kruskal-Wallis rank sum test (implemented in
\code{\link{kruskal.test}}), where group differences can be quantified as
log-odds ratios. If each observation forms its own block for
\eqn{K}-samples, an analogue to \code{\link{friedman.test}} is obtained.
Unlike these classical implementations, \code{free1way} allows discrete
binary or ordered outcomes to be analysed in the same spirit, for example
for binary paired comparisons (as in \code{\link{mcnemar.test}}).

}
\value{
An object of class \code{free1way} with corresponding \code{logLik},
\code{coef} (which returns \eqn{\delta_k - \mu_k}, not \eqn{\delta_k}
whenever \code{mu != 0}, because \eqn{\mu_k} enters the model as an offset
term for the \eqn{k}th group), \code{vcov}, \code{summary}, and \code{confint} methods.
}
\references{
  \bibshow{*}
}
\examples{

## Kruskal-Wallis test
kruskal.test(Ozone ~ Month, data = airquality)
kt <- free1way(Ozone ~ Month, data = airquality)
print(kt)
# log-odds ratios for comparison with control
coef(kt)
# Wald inference
summary(kt)
confint(kt, test = "Wald")

## Friedman test
example(friedman.test, echo = FALSE)
me <- colnames(RoundingTimes)
d <- expand.grid(me = factor(me, labels = me, levels = me),
                 id = factor(seq_len(nrow(RoundingTimes))))
d$time <- c(t(RoundingTimes))
# global p-value identical
friedman.test(RoundingTimes)
ft <- free1way(time ~ me | id, data = d)
print(ft)
coef(ft)
# Wald inference
summary(ft)
confint(ft, test = "Wald")

## McNemar test
## paired binary observations
example(mcnemar.test, echo = FALSE)
# set-up data frame with survey outcomes for voters
s <- gl(2, 1, labels = dimnames(Performance)[[1L]])
survey <- gl(2, 1, labels = c("1st", "2nd"))
nvoters <- c(Performance)
x <- expand.grid(survey = survey, voter = factor(seq_len(sum(nvoters))))
x$performance <- c(rep(s[c(1, 1)], nvoters[1]), rep(s[c(2, 1)], nvoters[2]),
                   rep(s[c(1, 2)], nvoters[3]), rep(s[c(2, 2)], nvoters[4]))
# note that only those voters changing their minds are relevant
mcn <- free1way(xtabs(~ performance + survey + voter, data = x))
# same result as mcnemar.test w/o continuity correction
print(mcn)
# X^2 statistic
summary(mcn, test = "Permutation")$statistic^2
mcnemar.test(Performance, correct = FALSE)
# Wald inference
summary(mcn)
confint(mcn, test = "Wald")

## Mantel-Haenszel test w/o continuity correction, 
## Departments are blocks
mantelhaen.test(UCBAdmissions, correct = FALSE)
mh <- free1way(UCBAdmissions)
print(mh)
# common odds-ratio, with score interval
exp(coef(mh))
exp(confint(mh, test = "Rao"))
# looking at department-specific 
# confidence intervals for log-odds ratios 
# it seems Dept A is out of line
apply(UCBAdmissions, MARGIN = 3,  
      FUN = function(x) confint(free1way(as.table(x))))

## Mantel-Haenszel test treats variables as
## unordered, free1way allows ordered responses
example(mantelhaen.test, echo = FALSE)
# Does distribution of job satisfaction (ordered) depend on income
# in a stratified proportional odds model?
# Job Satisfaction is second in array but needs to be first
# for free1way to treat it as ordered response
ft <- free1way(aperm(Satisfaction, perm = c(2, 1, 3)))
summary(ft)

}
\keyword{htest}
