\name{SweaveUtils}
\alias{SweaveUtils}
\alias{RtangleWritedoc}
\alias{RweaveLatexOptions}
\alias{RweaveChunkPrefix}
\alias{SweaveHooks}
\alias{RweaveTryStop}
\alias{RweaveEvalWithOpt}
\title{Sweave Driver Utilities}
\description{
  These functions are handy for writing Sweave drivers and currently not
  documented. Look at the source code of the Sweave Latex driver (in this
  package) or the HTML driver (in the R2HTML package from CRAN) to see
  how they can be used.
}
\usage{
RtangleWritedoc(object, chunk)
RweaveChunkPrefix(options)
RweaveEvalWithOpt(expr, options)
RweaveLatexOptions(options)
RweaveTryStop(err, options)
SweaveHooks(options, run = FALSE, envir = .GlobalEnv)
}
\author{Friedrich Leisch}
\keyword{utilities}
\keyword{internal}
