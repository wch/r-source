% File src/library/stats/man/extractAIC.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

%-- This page by Martin Maechler,  improvements welcome!
\name{extractAIC}
\title{Extract AIC from a Fitted Model}
%
\alias{extractAIC}
\usage{
extractAIC(fit, scale, k = 2, \dots)
}
\arguments{
  \item{fit}{fitted model, usually the result of a fitter like
    \code{\link{lm}}.}
 \item{scale}{optional numeric specifying the scale parameter of the
   model, see \code{scale} in \code{\link{step}}.  Currently only used
   in the \code{"lm"} method, where \code{scale} specifies the estimate
   of the error variance, and \code{scale = 0} indicates that it is to
   be estimated by maximum likelihood.
 }
 \item{k}{numeric specifying the \sQuote{weight} of the
   \emph{equivalent degrees of freedom} (\eqn{\equiv}{=:} \code{edf})
   part in the AIC formula.}
 \item{\dots}{further arguments (currently unused in base \R).}
}
%-- Source in ../R/add.R
\description{
  Computes the (generalized) Akaike \bold{A}n \bold{I}nformation
  \bold{C}riterion for a fitted parametric model.
}
\details{
  This is a generic function, with methods in base \R for classes
  \code{"aov"}, \code{"glm"} and \code{"lm"} as well as for
  \code{"negbin"} (package \CRANpkg{MASS}) and \code{"coxph"} and
  \code{"survreg"} (package \CRANpkg{survival}).

  The criterion used is
  \deqn{AIC = - 2\log L +  k \times \mbox{edf},}{AIC = - 2*log L +  k * edf,}
  where \eqn{L} is the likelihood and \code{edf} the equivalent degrees
  of freedom (i.e., the number of free parameters for usual parametric
  models) of \code{fit}.

  For linear models with unknown scale (i.e., for \code{\link{lm}} and
  \code{\link{aov}}), \eqn{-2\log L}{-2 log L} is computed from the
  \emph{deviance} and uses a different additive constant to
  \code{\link{logLik}} and hence \code{\link{AIC}}.  If \eqn{RSS}
  denotes the (weighted) residual sum of squares then \code{extractAIC}
  uses for \eqn{- 2\log L}{-2 log L} the formulae \eqn{RSS/s - n} (corresponding
  to Mallows' \eqn{C_p}{Cp}) in the case of known scale \eqn{s} and
  \eqn{n \log (RSS/n)}{n log (RSS/n)} for unknown scale.
  \code{\link{AIC}} only handles unknown scale and uses the formula
  \eqn{n \log (RSS/n) + n + n \log 2\pi - \sum \log w}{n*log(RSS/n) + n + n*log 2pi - sum(log w)}
  where \eqn{w} are the weights.  Further \code{AIC} counts the scale
  estimation as a parameter in the \code{edf} and \code{extractAIC} does not.

  For \code{glm} fits the family's \code{aic()} function is used to
  compute the AIC: see the note under \code{logLik} about the
  assumptions this makes.

  \code{k = 2} corresponds to the traditional AIC, using \code{k =
    log(n)} provides the BIC (Bayesian IC) instead.

  Note that the methods for this function may differ in their
  assumptions from those of methods for \code{\link{AIC}} (usually
  \emph{via} a method for \code{\link{logLik}}).  We have already
  mentioned the case of \code{"lm"} models with estimated scale, and
  there are similar issues in the \code{"glm"} and \code{"negbin"}
  methods where the dispersion parameter may or may not be taken as
  \sQuote{free}.  This is immaterial as \code{extractAIC} is only used
  to compare models of the same class (where only differences in AIC
  values are considered).
}
\note{
  This function is used in \code{\link{add1}}, \code{\link{drop1}}
  and \code{\link{step}} and the similar functions in package
  \CRANpkg{MASS} from which it was adopted.
}
\value{
  A numeric vector of length 2, with first and second elements giving

  \item{edf}{the \sQuote{\bold{e}quivalent \bold{d}egrees of \bold{f}reedom}
    for the fitted model \code{fit}.}

  \item{AIC}{the (generalized) Akaike Information Criterion for \code{fit}.}
}
%-- Source in ../R/add.R
\author{B. D. Ripley}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.}
  New York: Springer (4th ed).
}
\seealso{
  \code{\link{AIC}}, \code{\link{deviance}}, \code{\link{add1}},
  \code{\link{step}}
}
\examples{\donttest{
utils::example(glm)
extractAIC(glm.D93)  #>>  5  15.129
}}
\keyword{models}
