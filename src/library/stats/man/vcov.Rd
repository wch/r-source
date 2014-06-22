% File src/library/stats/man/vcov.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{vcov}
\alias{vcov}
\alias{vcov.lm}
\alias{vcov.glm}
\alias{vcov.summary.lm}
\alias{vcov.summary.glm}
\alias{vcov.lme}
\alias{vcov.gls}
\title{
  Calculate Variance-Covariance Matrix for a Fitted Model Object
}
\description{
  Returns the variance-covariance matrix of the main parameters of
  a fitted model object.
}
\usage{
vcov(object, \dots)
}
\arguments{
  \item{object}{
    a fitted model object, typically.  Sometimes also a
    \code{\link{summary}()} object of such a fitted model.
  }
  \item{\dots}{
    additional arguments for method functions.  For the
    \code{\link{glm}} method this can be used to pass a
    \code{dispersion} parameter.
  }
}
\details{
  This is a generic function.
  Functions with names beginning in \code{vcov.} will be
  methods for this function.
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
}
\value{
  A matrix of the estimated covariances between the parameter estimates
  in the linear or non-linear predictor of the model.  This should have
  row and column names corresponding to the parameter names given by the
  \code{\link{coef}} method.
}
\keyword{models}
\keyword{nonlinear}
