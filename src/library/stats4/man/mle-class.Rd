% File src/library/stats4/man/mle-class.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{mle-class}
\docType{class}
\title{Class \code{"mle"} for Results of Maximum Likelihood Estimation}
\alias{mle-class}
\alias{nobs,mle-method}
\description{This class encapsulates results of a generic maximum
  likelihood procedure.}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("mle", \dots)}, but
  most often as the result of a call to \code{\link{mle}}.
}
\section{Slots}{
  \describe{
    \item{\code{call}:}{Object of class \code{"language"}.
      The call to \code{\link{mle}}.}
    \item{\code{coef}:}{Object of class \code{"numeric"}.  Estimated
      parameters.}
    \item{\code{fullcoef}:}{Object of class \code{"numeric"}.
      Fixed and estimated parameters.}
    \item{\code{vcov}:}{Object of class \code{"matrix"}.  Approximate
      variance-covariance matrix.}
    \item{\code{min}:}{Object of class \code{"numeric"}.  Minimum value
      of objective function.}
    \item{\code{details}:}{a \code{"\link{list}"}, as returned from
      \code{\link{optim}}.}
    \item{\code{minuslogl}:}{Object of class \code{"function"}.  The
      negative loglikelihood function.}
    \item{\code{nobs}:}{\code{"\link{integer}"} of length one.  The
      number of observations (often \code{NA}, when not set in call
      explicitly).}
    \item{\code{method}:}{Object of class \code{"character"}.  The
      optimization method used.}
  }
}
\section{Methods}{
  \describe{
    \item{confint}{\code{signature(object = "mle")}: Confidence
      intervals from likelihood profiles.}
    \item{logLik}{\code{signature(object = "mle")}: Extract maximized
      log-likelihood.}
    \item{profile}{\code{signature(fitted = "mle")}: Likelihood profile
      generation.}
    \item{nobs}{\code{signature(object = "mle")}: Number of
      observations, here simply accessing the \code{nobs} slot mentioned above.}
    \item{show}{\code{signature(object = "mle")}: Display object briefly.}
    \item{summary}{\code{signature(object = "mle")}: Generate object summary.}
    \item{update}{\code{signature(object = "mle")}:  Update fit.}
    \item{vcov}{\code{signature(object = "mle")}: Extract
      variance-covariance matrix.}
  }
}
\keyword{classes}
