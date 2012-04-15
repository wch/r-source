% File src/library/stats/man/checkMFClasses.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{.checkMFClasses}
\alias{.checkMFClasses}
\alias{.MFclass}
\alias{.getXlevels}
\title{Functions to Check the Type of Variables passed to Model Frames}
\description{
  \code{.checkMFClasses} checks if the variables used in a predict
  method agree in type with those used for fitting.

  \code{.MFclass} categorizes variables for this purpose.
}
\usage{
.checkMFClasses(cl, m, ordNotOK = FALSE)
.MFclass(x)
.getXlevels(Terms, m)
}
\arguments{
  \item{cl}{a character vector of class descriptions to match.}
  \item{m}{a model frame.}
  \item{x}{any \R object.}
  \item{ordNotOK}{logical: are ordered factors different?}
  \item{Terms}{a \code{terms} object.}
}
\details{
  For applications involving \code{model.matrix} such as linear models
  we do not need to differentiate between ordered factors and factors as
  although these affect the coding, the coding used in the fit is
  already recorded and imposed during prediction.  However, other
  applications may treat ordered factors differently:
  \code{\link[rpart]{rpart}} does, for example.
}
\value{
  \code{.MFclass} returns a character string, one of \code{"logical"},
  \code{"ordered"}, \code{"factor"}, \code{"numeric"}, \code{"nmatrix.*"}
  (a numeric matrix with a number of columns appended) or \code{"other"}.

  \code{.getXlevels} returns a named character vector, or \code{NULL}.
}
\keyword{utilities}
