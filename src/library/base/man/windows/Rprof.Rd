\name{Rprof}
\alias{Rprof}
\title{Enable Profiling of R's Execution}
\description{
  Enable or disable profiling of the execution of \R expressions.
}
\usage{
Rprof(filename = "Rprof.out", append = FALSE, interval = 0.02)
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
  
  Profiling works by writing out the call stack every \code{interval}
  seconds, to the file specified.   Either the \code{\link{summaryRprof}}
  function or the Perl script \code{Rcmd Rprof}
  can be used to process the output file to produce a summary of the
  usage; use \code{Rcmd Rprof --help} for usage information.

  Exactly what the time interval measures is subtle: it is time that the
  \R process is running and executing an \R command.  It is not however just
  CPU time, for if \code{readline()} is waiting for input, that counts
  (on Windows, but not on Unix).

  Note that the timing interval cannot be too small, for the time spent
  in each profiling step is added to the interval.  What is feasible is
  machine-dependent, but 10ms seems as small as advisable on a 1GHz machine.
}
\note{
  Using \code{Rcmd Rprof} needs Windows Perl to be installed.
}
\seealso{
  The chapter on ``Tidying and profiling R code'' in ``Writing \R
  Extensions'' (see the \file{doc/manual} subdirectory of the \R source
  tree).

  \code{\link{summaryRprof}}
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
## Post-process the output by
##   Rcmd Rprof Rprof.out
## at the command prompt.
}
}
\keyword{utilities}
