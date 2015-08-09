% File src/library/methods/man/methods-package.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{methods-package}
\alias{methods-package}
\docType{package}
\title{
Formal Methods and Classes
}
\description{
Formally defined methods and classes for R objects, plus
other programming tools, as described in the references.
}
\details{
  This package provides the \sQuote{S4} or \sQuote{S version 4}
  approach to methods and classes in a functional language.

  See the documentation entries \code{\link{Classes}},
  \code{\link{Methods}}, and \code{\link{GenericFunctions}} for general discussion of these topics, at a
  fairly technical level.  Links from those pages, and the
  documentation of \code{\link{setClass}} and \code{\link{setMethod}}
  cover the main programming tools needed.

  For a complete
  list of functions and classes, use \code{library(help="methods")}.
}

\references{
 Chambers, John M. (2008)
 \emph{Software for Data Analysis: Programming with R}
  Springer.  (For the R version.)

 Chambers, John M. (1998)
 \emph{Programming with Data}
 Springer (For the original S4 version.)
}
\author{
R Core Team

Maintainer: R Core Team \email{R-core@r-project.org}
}
\keyword{ package }
\keyword{ methods }
