\name{Rprof}
\alias{Rprof}
\title{Enable Profiling of R's Execution}
\description{
  Enable or disable profiling of the execution of \R expressions.
}
\usage{
Rprof(filename = "Rprof.out", append = TRUE, interval = 0.02)
}
\arguments{
  \item{filename}{
    The file to be used for recording the profiling results.
    Set to \code{NULL} or \code{""} to disable profiling.
  }
  \item{append}{
    logical: should the file be over-written or appended to?
  }
  \item{interval}{
    real: time interval between samples.
  }
}
\details{
  Enabling profiling automatically disables any existing profiling to
  another or the same file.
  
  Profiling works by writing out the call stack every \code{intervall}
  seconds, to the file specified.  The Perl script \code{R CMD Rprof} can
  be used to process the output file to produce a summary of the usage.
}
\note{
  Profiling is not available on all platforms, and is not enabled by
  default.  Configure \R with \code{--enable-R-profiling} to see if
  it can be used on your system.

  As \R profiling uses the same mechanisms as C profiling, the two
  cannot be used together, so do not use \code{Rprof} in an executable
  built for profiling.
}

\examples{
\dontrun{Rprof()
## some code to be profiled
Rprof(NULL)
## some code NOT to be profiled
Rprof(append=TRUE)
## some code to be profiled
Rprof(NULL)
\dots
## post-process the output by
R CMD Rprof Rprof.out}
}
\keyword{utility}
