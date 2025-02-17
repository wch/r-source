% File src/library/stats4/man/plot-methods.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{plot-methods}
\docType{methods}
\alias{plot-methods}
\alias{plot,ANY,ANY-method}
\alias{plot,profile.mle,missing-method}
\title{Methods for Function \code{plot} in Package \pkg{stats4}}
\description{
  Plot profile likelihoods for \code{"mle"} objects.
}
\usage{
\S4method{plot}{profile.mle,missing}(x, levels, conf = c(99, 95, 90, 80, 50)/100, nseg = 50,
     absVal = TRUE, \dots)
}
\arguments{
 \item{x}{an object of class \code{"profile.mle"} }
 \item{levels}{levels, on the scale of the absolute value of a t
     statistic, at which to interpolate intervals.  Usually \code{conf}
     is used instead of giving \code{levels} explicitly.}
 \item{conf}{a numeric vector of confidence levels for profile-based
     confidence intervals on the parameters.}
 \item{nseg}{an integer value giving the number of segments to use in
     the spline interpolation of the profile t curves.}
 \item{absVal}{a logical value indicating whether or not the plots
     should be on the scale of the absolute value of the profile t.
     Defaults to \code{TRUE}.}
 \item{\dots}{other arguments to the \code{plot} function can be passed here.}
}
\section{Methods}{
  \describe{
    \item{\code{signature(x = "ANY", y = "ANY")}}{Generic function: see
      \code{\link[graphics]{plot}}.}
    \item{\code{signature(x = "profile.mle", y = "missing")}}{Plot
      likelihood profiles for \code{x}.}
  }
}
\keyword{methods}
