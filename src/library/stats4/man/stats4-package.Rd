% File src/library/stats4/man/stats4-package.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{stats4-package}
\alias{stats4-package}
\alias{stats4}
\docType{package}
\title{Statistical Functions using S4 Classes}
\description{Statistical Functions using S4 classes.
}
\details{
  This package contains functions and classes for statistics
  using the \link[methods:methods-package]{S version 4} class system.

  The methods currently support maximum likelihood (function
  \code{\link{mle}()} returning class \code{"\linkS4class{mle}"}),
  including methods for \code{\link[=AIC-methods]{AIC}} and \code{\link{BIC}}
  for model selection.
}
\author{
R Development Core Team and contributors worldwide

Maintainer: R Core Team <R-core@r-project.org>
}
\keyword{ package }
\keyword{ models }
