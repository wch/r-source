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
  This package provides the \dQuote{S4} or \dQuote{S version 4}
  approach to methods and classes in a functional language.

  For basic use of the techniques, start with \link{Introduction} and
  follow the links there to the key functions for programming, notably
  \code{\link{setClass}} and \code{\link{setMethod}}. 

  For detailed discussion, see the documentation entries \code{\link{Classes_Details}},
  \code{\link{Methods_Details}}, and \code{\link{GenericFunctions}} for these topics, at a
  fairly technical level.

  For a complete
  list of functions and classes, use \code{library(help="methods")}.
}

\references{
 Chambers, John M. (2016)
 \emph{Extending R},
  Chapman & Hall.
(Chapters 9 and 10.)

 Chambers, John M. (2008)
 \emph{Software for Data Analysis: Programming with R}
  Springer.  (Chapter 10 has some additional details.)

}
\author{
R Core Team

Maintainer: R Core Team \email{R-core@r-project.org}
}
\keyword{ package }
\keyword{ methods }
