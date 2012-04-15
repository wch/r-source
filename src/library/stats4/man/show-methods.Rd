% File src/library/stats4/man/show-methods.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{show-methods}
\docType{methods}
\alias{show-methods}
\alias{show,mle-method}
\alias{show,summary.mle-method}
\title{Methods for Function \code{show} in Package \pkg{stats4}}
\description{Show objects of classes \code{mle} and \code{summary.mle}}
\section{Methods}{
  \describe{
    \item{\code{signature(object = "mle")}}{Print simple summary of
      \code{mle} object.  Just the coefficients and the call.}
    \item{\code{signature(object = "summary.mle")}}{Shows call, table of
      coefficients and standard errors, and \eqn{-2 \log L}{-2 log L}.}
  }
}
\keyword{methods}
