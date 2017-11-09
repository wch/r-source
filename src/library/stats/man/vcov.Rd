% File src/library/stats/man/vcov.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{vcov}
\title{Calculate Variance-Covariance Matrix for a Fitted Model Object}
\alias{vcov}
\alias{vcov.aov}
\alias{vcov.lm}
\alias{vcov.glm}
\alias{vcov.summary.lm}
\alias{vcov.summary.glm}
\alias{vcov.lme}
\alias{vcov.gls}
\alias{.vcov.aliased}
\description{
  Returns the variance-covariance matrix of the main parameters of
  a fitted model object.  The \dQuote{main} parameters of model
  correspond to those returned by \code{\link{coef}}, and typically do
  not contain a nuisance scale parameter (\code{\link{sigma}}).
}
\usage{
vcov(object, \dots)
\S3method{vcov}{lm}(object, complete = TRUE, \dots)
## and also for '[summary.]glm' and 'mlm'
\S3method{vcov}{aov}(object, complete = FALSE, \dots)

.vcov.aliased(aliased, vc, complete = TRUE)
}
\arguments{
  \item{object}{
    a fitted model object, typically.  Sometimes also a
    \code{\link{summary}()} object of such a fitted model.
  }
  \item{complete}{for the \code{aov}, \code{lm}, \code{glm}, \code{mlm}, and where
    applicable \code{summary.lm} etc methods: logical indicating if the
    full variance-covariance matrix should be returned also in case of
    an over-determined system where some coefficients are undefined and
    \code{\link{coef}(.)} contains \code{NA}s correspondingly.   When
    \code{complete = TRUE},  \code{vcov()} is compatible with
    \code{coef()} also in this singular case.}
  \item{\dots}{
    additional arguments for method functions.  For the
    \code{\link{glm}} method this can be used to pass a
    \code{dispersion} parameter.}
  %% for .vcov.aliased() :
  \item{aliased}{a \code{\link{logical}} vector typically identical to
    \code{is.na(coef(.))} indicating which coefficients are \sQuote{aliased}.}
  \item{vc}{a variance-covariance matrix, typically \dQuote{incomplete},
    i.e., with no rows and columns for aliased coefficients.}
}
\details{
  \code{vcov()} is a generic function and functions with names beginning
  in \code{vcov.} will be methods for this function.
  Classes with methods for this function include:
  \code{lm}, \code{mlm}, \code{glm}, \code{nls},
  \code{summary.lm}, \code{summary.glm},
  \code{negbin}, \code{polr}, \code{rlm} (in package \CRANpkg{MASS}),
  \code{multinom} (in package \CRANpkg{nnet})
  \code{gls}, \code{lme} (in package \CRANpkg{nlme}),
  \code{coxph} and \code{survreg} (in package \CRANpkg{survival}).

  (\code{vcov()} methods for summary objects allow more
  efficient and still encapsulated access when both
  \code{summary(mod)} and \code{vcov(mod)} are needed.)

  \code{.vcov.aliased()} is an auxiliary function useful for
  \code{vcov} method implementations which have to deal with singular
  model fits encoded via NA coefficients: It augments a vcov--matrix
  \code{vc} by \code{\link{NA}} rows and columns where needed, i.e., when
  some entries of \code{aliased} are true and \code{vc} is of smaller dimension
  than \code{length(aliased)}.
}
\value{
  A matrix of the estimated covariances between the parameter estimates
  in the linear or non-linear predictor of the model.  This should have
  row and column names corresponding to the parameter names given by the
  \code{\link{coef}} method.

  When some coefficients of the (linear) model are undetermined and
  hence \code{NA} because of linearly dependent terms (or an
  \dQuote{over specified} model), also called
  \dQuote{aliased}, see \code{\link{alias}}, then since \R version 3.5.0,
  \code{vcov()} (iff \code{complete = TRUE}, i.e., by default for
  \code{lm} etc, but not for \code{aov}) contains corresponding rows and
  columns of \code{NA}s, wherever \code{\link{coef}()} has always
  contained such \code{NA}s.
}
\keyword{models}
\keyword{nonlinear}
