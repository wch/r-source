% File src/library/utils/man/SweaveUtils.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{SweaveUtils}
\alias{SweaveUtils}
\alias{RweaveChunkPrefix}
\alias{RweaveEvalWithOpt}
\alias{RweaveTryStop}
\alias{.RtangleCodeLabel}
\alias{SweaveHooks}
\title{Sweave Driver Utilities}
\description{
  Utility functions for Sweave drivers.
}
\usage{
RweaveChunkPrefix(options)

RweaveEvalWithOpt(expr, options)

RweaveTryStop(err, options)

.RtangleCodeLabel(chunk)

SweaveHooks(options, run = FALSE, envir = .GlobalEnv)
}
\arguments{
  \item{options}{named list of character strings.}
  \item{expr}{expression to be evaluated.}
  \item{err}{value of an expression or invisible object inheriting from
    class \code{"try-error"}.}
  \item{chunk}{text of a code chunk.}
  \item{run}{evaluate the hook functions?}
  \item{envir}{the environment in which the hook function is to be
    evaluated.}
}
\details{
  These are internal utility functions for the Sweave drivers. Look at
  the source code of the \code{\link{RweaveLatex}} and
  \code{\link{Rtangle}} drivers (in this package) or the HTML driver (in
  the \pkg{R2HTML} package from CRAN) to see how they can be used.
  
  \code{RweaveChunkPrefix} returns the prefix string for a chunk,
  following options \code{prefix} and \code{prefix.string} of the
  drivers.
  
  \code{RweaveEvalWithOpt} and \code{RweaveTryStop} handle the
  evaluation of code chunks and resulting errors, if any.

  \code{.RtangleCodeLabel} builds the label used in the annotations for
  an unlabeled code chunk.
  
  \code{SweaveHooks} evaluates (when \code{run = TRUE}) the hook
  functions and returns the names of the active hooks for a code chunk.
}
\author{
  Friedrich Leisch, with contributions by Seth Falcon and Vincent Goulet.
}
\seealso{
  The \code{\link{Sweave}}, \code{\link{RweaveLatex}} and
  \code{\link{Rtangle}} help pages.
}
\keyword{utilities}
\keyword{internal}
