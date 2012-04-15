% File src/library/stats4/man/vcov-methods.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{vcov-methods}
\docType{methods}
\alias{vcov-methods}
\alias{vcov,ANY-method}
\alias{vcov,mle-method}
\title{Methods for Function \code{vcov} in Package \pkg{stats4}}
\description{
  Extract the approximate variance-covariance matrix from \code{"mle"}
  objects.
}
\section{Methods}{
  \describe{
    \item{\code{signature(object = "ANY")}}{Generic function: see
      \code{\link[stats]{vcov}}.}
    \item{\code{signature(object = "mle")}}{Extract the estimated
      variance-covariance matrix for the estimated parameters (if any).}
  }
}
\keyword{methods}
