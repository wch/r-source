% File src/library/stats4/man/logLik-methods.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{logLik-methods}
\docType{methods}
\alias{logLik-methods}
\alias{logLik,ANY-method}
\alias{logLik,mle-method}
\title{Methods for Function \code{logLik} in Package \pkg{stats4}}
\description{
  Extract the maximized log-likelihood from \code{"mle"} objects.
}
\section{Methods}{
  \describe{
    \item{\code{signature(object = "ANY")}}{Generic function: see
      \code{\link[stats]{logLik}}.}
    \item{\code{signature(object = "mle")}}{Extract log-likelihood from
      the fit.}
  }
}
\note{
  The \code{mle} method does not know about the number of observations
  unless \code{nobs} was specified on the call and so may not be
  suitable for use with \code{\link{BIC}}.
}
\keyword{methods}
