% File src/library/stats4/man/coef-methods.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{coef-methods}
\docType{methods}
\alias{coef-methods}
\alias{coef,ANY-method}
\alias{coef,mle-method}
\alias{coef,summary.mle-method}
\title{Methods for Function \code{coef} in Package \pkg{stats4}}
\description{
  Extract the coefficient vector from \code{"mle"} objects.
}
\section{Methods}{
  \describe{
    \item{\code{signature(object = "ANY")}}{Generic function: see
      \code{\link[stats]{coef}}.}
    \item{\code{signature(object = "mle")}}{Extract the full coefficient
      vector (including any fixed coefficients) from the fit.}
    \item{\code{signature(object = "summary.mle")}}{Extract the
      coefficient vector and standard errors from the summary of the
      fit.}
  }
}
\keyword{methods}
