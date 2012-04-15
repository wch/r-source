% File src/library/stats4/man/summary-methods.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{summary-methods}
\docType{methods}
\alias{summary-methods}
\alias{summary,ANY-method}
\alias{summary,mle-method}
\title{Methods for Function \code{summary} in Package \pkg{stats4}}
\description{Summarize objects}
\section{Methods}{
  \describe{
    \item{\code{signature(object = "ANY")}}{Generic function}
    \item{\code{signature(object = "mle")}}{Generate a summary as an
      object of class \code{"summary.mle"}, containing estimates,
      asymptotic SE, and value of \eqn{-2 \log L}{-2 log L}.}
  }
}
\keyword{methods}
