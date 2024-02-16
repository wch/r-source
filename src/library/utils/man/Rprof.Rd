% File src/library/utils/man/Rprof.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2023 R Core Team
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
       line.profiling = FALSE, filter.callframes = FALSE,
       numfiles = 100L, bufsize = 10000L,
       event = c("default", "cpu", "elapsed"))
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
    real: distance (time interval) between samples in seconds.
  }
  \item{memory.profiling}{logical: write memory use information to the file?}
  \item{gc.profiling}{logical:  record whether \abbr{GC} is running?}
  \item{line.profiling}{logical:  write line locations to the file?}
  \item{filter.callframes}{logical: filter out intervening call frames
    of the call tree. See the filtering out call frames section.}
  \item{numfiles, bufsize}{integers: line profiling memory allocation}
  \item{event}{
    character: profiling event, character vector of length one,
    \code{"elapsed"} for elapsed (real, wall-clock) time and \code{"cpu"}
    for CPU time, both measured in seconds. \code{"default"} is the default
    event on the platform, one of the two. See the \sQuote{Details}.
  }
}
\details{
  Enabling profiling automatically disables any existing profiling to
  another or the same file.

  Profiling works by writing out the call stack every \code{interval}
  seconds (units of the profiling event), to the file specified.  Either the
  \code{\link{summaryRprof}} function or the wrapper script \command{R CMD
  Rprof} can be used to process the output file to produce a summary of the
  usage; use \command{R CMD Rprof --help} for usage information.

  Exactly what is measured is subtle and depends on the profiling event.

  With \code{"elapsed"} (the default and only supported event on Windows): it
  is time that the \R process is running and executing an \R command.  It is
  not however just CPU time, for if \code{readline()} is waiting for input,
  that counts as well.  It is also known as `elapsed' time.

  With \code{"cpu"} (the default on Unix and typically the preferred event
  for identifying performance bottlenecks), it is CPU time of the \R process,
  so for example excludes time when \R is waiting for input or for processes
  run by \code{\link{system}} to return.  It may go slower than
  \code{"elapsed"} when the process is often waiting for I/O to finish, but it
  may go faster with actively computing concurrent threads (say via \abbr{OpenMP})
  on a multi-core system.

  Note that the (timing) interval cannot be too small.  With \code{"cpu"},
  the time spent in each profiling step is currently added to the interval. 
  With all profiling events, the computation in each profiling step causes
  perturbation to the observed system and biases the results.  What is
  feasible is machine-dependent.  On Linux, R requires the interval to be at
  least 10ms, on all other platforms at least 1ms.  Shorter intervals will
  be rounded up with a warning.

  The \code{"default"} profiling event is \code{"elapsed"} on Windows and
  \code{"cpu"} on Unix. 

  Support for \code{"elapsed"} event on Unix is new and considered
  experimental.  To reduce the risk of missing a sample, R tries to use the
  (real-time) FIFO scheduling policy with the maximum scheduling priority
  for an internal thread which initiates collection of each sample.  If
  setting that priority fails, it tries to use the maximum scheduling
  priority of the current scheduling policy, falling back to the current
  scheduling parameters.  On Linux, regular users are typically not allowed
  to use the real-time scheduling priorities.  This can be usually allowed
  via PAM (e.g.  \file{/etc/security/limits.conf}), see the OS documentation
  for details.  The priorities only matter when profiling a system under
  high load.

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

\section{Filtering Out Call Frames}{
  Lazy evaluation makes the call stack more complex because intervening
  call frames are created between the time arguments are applied to a
  function, and the time they are effectively evaluated. When the call
  stack is represented as a tree, these intervening frames appear as
  sibling nodes. For instance, evaluating \code{try(EXPR)} produces the
  following call tree, at the time \code{EXPR} gets evaluated:

\preformatted{
1. +-base::try(EXPR)
2. | \-base::tryCatch(...)
3. |   \-base:::tryCatchList(expr, classes, parentenv, handlers)
4. |     \-base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
5. |       \-base:::doTryCatch(return(expr), name, parentenv, handler)
6. \-EXPR
}

  Lines 2 to 5 are intervening call frames, the last of which finally
  triggered evaluation of \code{EXPR}. Setting \code{filter.callframes}
  to \code{TRUE} simplifies the profiler output by removing all sibling
  nodes of intervening frames.

  The same kind of call frame filtering is applied with \code{eval()}
  frames.  When you call \code{eval()}, two frames are pushed on the
  stack to ensure a continuity between frames. Say we have these
  definitions:

\preformatted{
calling <- function() evaluator(quote(called()), environment())
evaluator <- function(expr, env) eval(expr, env)
called <- function() EXPR()
}

  \code{calling()} calls \code{called()} in its own environment, via
  \code{eval()}. The latter is called indirectly through
  \code{evaluator()}. The net effect of this code is identical to just
  calling \code{called()} directly, without the intermediaries. However,
  the full call stack looks like this:


\preformatted{
1. calling()
2. \-evaluator(quote(called()), environment())
3.   \-base::eval(expr, env)
4.     \-base::eval(expr, env)
5.       \-called()
6.         \-EXPR()
}

  When call frame filtering is turned on, the true calling environment
  of \code{called()} is looked up, and the filtered call stack looks like
  this:

\preformatted{
1. calling()
5. \-called()
6.   \-EXPR()
}

  If the calling environment is not on the stack, the function called by
  \code{eval()} becomes a root node. Say we have:

\preformatted{
calling <- function() evaluator(quote(called()), new.env())
}

  With call frame filtering we then get the following filtered call
  stack:

\preformatted{
5. called()
6. \-EXPR()
}

}
\note{
  \describe{
    \item{On Unix-alikes:}{
  Profiling is not available on all platforms.  By default,
  support for profiling is compiled in if possible -- configure \R with
  \option{--disable-R-profiling} to change this.

  As \R CPU profiling uses the same mechanisms as C profiling, the two
  cannot be used together, so do not use \code{Rprof(event = "cpu")}
  (the default) in an executable built for C-level profiling (such as
  using the GCC option \option{-p} or \option{-pg}).
    }
    \item{On Windows:}{
  \code{filename} can be a UTF-8-encoded filepath that cannot be translated to
  the current locale.
    }
  }

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
  \sQuote{Writing R Extensions}: \code{\link{RShowDoc}("R-exts")}.

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
## ...
## Now post-process the output as described in Details
}}
\keyword{utilities}
