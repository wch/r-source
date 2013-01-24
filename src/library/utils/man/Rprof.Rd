% File src/library/utils/man/Rprof.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2010 R Core Team
% Distributed under GPL 2 or later

\name{Rprof}
\alias{Rprof}
\title{Enable Profiling of R's Execution}
\description{
  Enable or disable profiling of the execution of \R expressions.
}
\usage{
Rprof(filename = "Rprof.out", append = FALSE, interval = 0.02,
       memory.profiling = FALSE, gc.profiling = FALSE, 
       line.profiling = FALSE, numfiles = 100L, bufsize = 10000L)
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
  \item{memory.profiling}{logical: write memory use information to the file?}
  \item{gc.profiling}{logical:  record whether GC is running?}
  \item{line.profiling}{logical:  write line locations to the file?}
  \item{numfiles, bufsize}{integers: line profiling memory allocation}
}
\details{
  Enabling profiling automatically disables any existing profiling to
  another or the same file.

  Profiling works by writing out the call stack every \code{interval}
  seconds, to the file specified.  Either the \code{\link{summaryRprof}}
  function or the wrapper script \command{R CMD Rprof} can be used to
  process the output file to produce a summary of the usage; use
  \command{R CMD Rprof --help} for usage information.

#ifdef windows
  Exactly what the time interval measures is subtle: it is time that the
  \R process is running and executing an \R command.  It is not however just
  CPU time, for if \code{readline()} is waiting for input, that counts
  (on Windows, but not on a Unix-alike).

  Note that the timing interval cannot be too small, for the time spent
  in each profiling step is added to the interval.  What is feasible is
  machine-dependent, but 10ms seemed as small as advisable on a 1GHz machine.
#endif
#ifdef unix
  How time is measured varies by platform: on a Unix-alike it is the CPU
  time of the \R process, so for example excludes time when \R is waiting
  for input or for processes run by \code{\link{system}} to return.

  Note that the timing interval cannot usefully be too small: once the
  timer goes off, the information is not recorded until the next timing
  click (probably in the range 1--10msecs).
#endif

  Functions will only be recorded in the profile log if they put a
  context on the call stack (see \code{\link{sys.calls}}).  Some
  \link{primitive} functions do not do so: specifically those which are
  of \link{type} \code{"special"} (see the \sQuote{R Internals} manual
  for more details).
  
  Individual statements will be recorded in the profile log if
  \code{line.profiling} is \code{TRUE}, and if the code being executed
  was parsed with source references.  See \code{\link{parse}} for a
  discussion of source references.  By default the statement locations
  are not shown in \code{\link{summaryRprof}}, but see that help page
  for options to enable the display.    
}
#ifdef unix
\note{
  Profiling is not available on all platforms.  By default,
  support for profiling is compiled in if possible -- configure \R with
  \option{--disable-R-profiling} to change this.

  As \R profiling uses the same mechanisms as C profiling, the two
  cannot be used together, so do not use \code{Rprof} in an executable
  built for C-level profiling.
}
#endif
#ifdef windows
\note{
  \code{filename} can be a UTF-8-encoded filepath that cannot be translated to
  the current locale.
}
#endif
\note{
The profiler interrupts R asynchronously, and it cannot allocate
memory to store results as it runs.  This affects line profiling,
which needs to store an unknown number of file pathnames.  The
\code{numfiles} and \code{bufsize} arguments control the size of 
pre-allocated buffers to hold these results:  the former counts the
maximum number of paths, the latter counts the numbers of bytes in
them.  If the profiler runs out of space it will skip recording the
line information for new files, and issue a warning when
\code{Rprof(NULL)} is called to finish profiling.
}
\seealso{
  The chapter on \dQuote{Tidying and profiling R code} in
  \dQuote{Writing \R Extensions} (see the \file{doc/manual} subdirectory
  of the \R source tree).

  \code{\link{summaryRprof}} to analyse the output file.

  \code{\link{tracemem}}, \code{\link{Rprofmem}} for other ways to track
  memory use.
}
\examples{
\dontrun{Rprof()
## some code to be profiled
Rprof(NULL)
## some code NOT to be profiled
Rprof(append = TRUE)
## some code to be profiled
Rprof(NULL)
\dots
## Now post-process the output as described in Details
}}
\keyword{utilities}
