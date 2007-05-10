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
 \item{k}{numeric specifying the \dQuote{weight} of the
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
  This is a generic function, with methods in base \R for \code{"aov"},
  \code{"coxph"}, \code{"glm"}, \code{"lm"}, \code{"negbin"}
  and \code{"survreg"} classes.

  The criterion used is
  \deqn{AIC = - 2\log L +  k \times \mbox{edf},}{AIC = - 2*log L +  k * edf,}
  where \eqn{L} is the likelihood and \code{edf} the equivalent degrees
  of freedom (i.e., the number of free parameters for usual parametric
  models) of \code{fit}.

  For linear models with unknown scale (i.e., for \code{\link{lm}} and
  \code{\link{aov}}), \eqn{-2\log L} is computed from the
  \emph{deviance} and uses a different additive constant to
  \code{\link{logLik}} and hence \code{\link{AIC}}.  If \eqn{RSS}
  denotes the (weighted) residual sum of squares then \code{extractAIC}
  uses for \eqn{- 2\log L} the formulae \eqn{RSS/s - n} (corresponding
  to Mallows' \eqn{C_p}{Cp}) in the case of known scale \eqn{s} and
  \eqn{n \log (RSS/n)} for unknown scale. \code{\link{AIC}} only handles
  unknown scale and uses the formula
  \eqn{n \log (RSS/n) - n + n \log 2\pi - \sum \log w}
  where \eqn{w} are the weights.

  For \code{glm} fits the family's \code{aic()} function to compute the
  AIC: see the note under \code{logLik} about the assumptions this makes.

  \code{k = 2} corresponds to the traditional AIC, using \code{k =
    log(n)} provides the BIC (Bayesian IC) instead.
}
\note{
  This function is used in \code{\link{add1}}, \code{\link{drop1}}
  and \code{\link{step}} and similar functions in package \pkg{MASS}
  from which it was adopted.
}
\value{
  A numeric vector of length 2, giving

  \item{edf}{the \dQuote{\bold{e}quivalent \bold{d}egrees of \bold{f}reedom}
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
\examples{
utils::example(glm)
extractAIC(glm.D93)#>>  5  15.129
}
\keyword{models}

