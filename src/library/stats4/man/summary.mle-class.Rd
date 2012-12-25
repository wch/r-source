% File src/library/stats4/man/summary.mle-class.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{summary.mle-class}
\docType{class}
\alias{summary.mle-class}
\title{Class \code{"summary.mle"}, Summary of \code{"mle"} Objects}
\description{Extract of "mle" object}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("summary.mle",
  ...)}, but most often by invoking \code{summary} on an "mle" object.
They contain values meant for printing by \code{show}.
}
\section{Slots}{
  \describe{
    \item{\code{call}:}{Object of class \code{"language"} The call that
      generated the "mle" object.}
    \item{\code{coef}:}{Object of class \code{"matrix"}. Estimated
      coefficients and standard errors }
    \item{\code{m2logL}:}{Object of class \code{"numeric"}. Minus twice
      the log likelihood.}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "summary.mle")}: Pretty-prints
      \code{object}  }
    \item{coef}{\code{signature(object = "summary.mle")}: Extracts the
    contents of the \code{coef} slot}
  }
}
\seealso{
  \code{\link{summary}}, \code{\link{mle}}, \code{\link{mle-class}}
}
\keyword{classes}
