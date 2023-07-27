% File src/library/utils/man/SweaveUtils.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{SweaveUtils}
\alias{RtangleWritedoc}
\alias{RweaveChunkPrefix}
\alias{RweaveEvalWithOpt}
\alias{RweaveLatexFinish}
\alias{RtangleFinish}
\alias{RweaveLatexOptions}
\alias{RweaveLatexWritedoc}
\alias{RweaveTryStop}
\alias{SweaveHooks}
\alias{SweaveUtils}
\alias{makeRweaveLatexCodeRunner}
\alias{RtangleRuncode}
\alias{.RtangleCodeLabel}
\title{Sweave Driver Utilities}
\description{
  These functions are handy for writing Sweave drivers and are
  considered internal and hence not documented.  Look at the source code
  of the Sweave \LaTeX driver (in this
  package) or the HTML driver (in the \pkg{R2HTML} package from CRAN) to see
  how they can be used.
}
\usage{
RtangleWritedoc(object, chunk)
RweaveChunkPrefix(options)
RweaveEvalWithOpt(expr, options)
RweaveLatexFinish(object, error = FALSE)
RtangleFinish(object, error = FALSE)
RweaveLatexOptions(options)
RweaveLatexWritedoc(object, chunk)
RweaveTryStop(err, options)
SweaveHooks(options, run = FALSE, envir = .GlobalEnv)
makeRweaveLatexCodeRunner(evalFunc = RweaveEvalWithOpt)
RtangleRuncode(object, chunk, options)
.RtangleCodeLabel(chunk)
}
\author{Friedrich Leisch, with contributions by Seth Falcon}
\seealso{The \code{\link{Sweave}}, \code{\link{RweaveLatex}} help pages.
}
\keyword{utilities}
\keyword{internal}
