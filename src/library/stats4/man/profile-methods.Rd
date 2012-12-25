% File src/library/stats4/man/profile-methods.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Team
% Distributed under GPL 2 or later

\name{profile-methods}
\docType{methods}
\alias{profile-methods}
\alias{profile,ANY-method}
\alias{profile,mle-method}
\title{Methods for Function \code{profile} in Package \pkg{stats4}}
\description{Profile likelihood for \code{"mle"} objects.}
\usage{
\S4method{profile}{mle}(fitted, which = 1:p, maxsteps = 100, alpha = 0.01,
        zmax = sqrt(qchisq(1 - alpha, 1L)), del = zmax/5,
        trace = FALSE, ...)
}
\arguments{
  \item{fitted}{Object to be profiled}
  \item{which}{Optionally select subset of parameters to profile.}
  \item{maxsteps}{Maximum number of steps to bracket \code{zmax}.}
  \item{alpha}{Significance level corresponding to \code{zmax}, based on
    a Scheffe-style multiple testing interval.  Ignored
    if \code{zmax} is specified.}
  \item{zmax}{Cutoff for the profiled value of the signed root-likelihood.}
  \item{del}{Initial stepsize on root-likelihood scale.}
  \item{trace}{Logical. Print intermediate results.}
  \item{\dots}{Currently unused.}
}
\details{
  The profiling algorithm tries to find an approximately evenly spaced
  set of at least five parameter values (in each direction from the
  optimum) to cover
  the root-likelihood function. Some care is taken to try and get sensible
  results in cases of high parameter curvature. Notice that it may not
  always be possible to obtain the cutoff value, since the likelihood
  might level off.
}
\value{
  An object of class \code{"profile.mle"}, see
  \code{"profile.mle-class"}.
}

\section{Methods}{
  \describe{
    \item{\code{signature(fitted = "ANY")}}{Generic function: see
      \code{\link[stats]{profile}}.}
    \item{\code{signature(fitted = "mle")}}{Profile the likelihood in
      the vicinity of the optimum of an \code{"mle"} object.}
  }
}
\keyword{methods}
