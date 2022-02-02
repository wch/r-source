% File src/library/stats4/man/confint-methods.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{confint-methods}
\docType{methods}
\alias{confint-methods}
\alias{confint,ANY-method}
\alias{confint,profile.mle-method}
\alias{confint,mle-method}
\title{Methods for Function \code{confint} in Package \pkg{stats4}}
\description{Generate confidence intervals}
\section{Methods}{
  \describe{
    \item{\code{signature(object = "ANY")}}{Generic function: see
      \code{\link[stats]{confint}}.}
    \item{\code{signature(object = "mle")}}{First generate profile and
      then confidence intervals from the profile.}
    \item{\code{signature(object = "profile.mle")}}{Generate confidence
      intervals based on likelihood profile.}
  }
}
\keyword{methods}
