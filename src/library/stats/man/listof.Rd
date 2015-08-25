% File src/library/stats/man/listof.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{listof}
\alias{listof}

\title{A Class for Lists of (Parts of) Model Fits}
\description{
  Class \code{"listof"} is used by \code{\link{aov}} and the \code{"lm"}
  method of \code{\link{alias}} for lists of model fits or parts
  thereof.  It is simply a list with an assigned class to control the way
  methods, especially printing, act on it.

  It has a \code{\link{coef}} method in this package (which returns an
  object of this class), and \code{[} and \code{print} methods in
  package \pkg{base}.
}
