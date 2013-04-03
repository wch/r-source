% File src/library/parallel/man/unix/mcparallel.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2009-11 R Core Team
% Distributed under GPL 2 or later

\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{mcparallel}
\alias{mccollect}
\alias{mcparallel}

\title{Evaluate an \R Expression Asynchronously in a Separate Process}
\description{
  These functions are based on forking and so are not available on Windows.

  \code{mcparallel} starts a parallel \R process which evaluates the
  given expression.

  \code{mccollect} collects results from one or more parallel processes.
}
\usage{
mcparallel(expr, name, mc.set.seed = TRUE, silent = FALSE,
           mc.affinity = NULL, mc.interactive = FALSE)

mccollect(jobs, wait = TRUE, timeout = 0, intermediate = FALSE)
}
\arguments{
  \item{expr}{expression to evaluate (do \emph{not} use any on-screen
    devices or GUI elements in this code).}
  \item{name}{an optional name (character vector of length one) that can
    be associated with the job.}
  \item{mc.set.seed}{logical: see section \sQuote{Random numbers}.}
  \item{silent}{if set to \code{TRUE} then all output on stdout will be
    suppressed (stderr is not affected).}
  \item{mc.affinity}{either a numeric vector specifying CPUs to restrict
    the child process to (1-based) or \code{NULL} to not modify the CPU
    affinity}
  \item{mc.interactive}{logical, if \code{TRUE} or \code{FALSE} then the
    child process will be set as interactive or non-interactive
    respectively. If \code{NA} then the child process will inherit the
    interactive flag from the parent.}
  \item{jobs}{list of jobs (or a single job) to collect results
    for.  Alternatively \code{jobs} can also be an integer vector of
    process IDs.  If omitted \code{collect} will wait for all currently
    existing children.}
  \item{wait}{if set to \code{FALSE} it checks for any results that are
    available within \code{timeout} seconds from now, otherwise it waits
    for all specified jobs to finish.}
  \item{timeout}{timeout (in seconds) to check for job results -- applies
    only if \code{wait} is \code{FALSE}.}
  \item{intermediate}{\code{FALSE} or a function which will be called while
    \code{collect} waits for results.  The function will be called with one
    parameter which is the list of results received so far.}
}
\details{
  \code{mcparallel} evaluates the \code{expr} expression in parallel to
  the current \R process.  Everything is shared read-only (or in fact
  copy-on-write) between the parallel process and the current process,
  i.e. no side-effects of the expression affect the main process.  The
  result of the parallel execution can be collected using
  \code{mccollect} function.

  \code{mccollect} function collects any available results from parallel
  jobs (or in fact any child process).  If \code{wait} is \code{TRUE}
  then \code{collect} waits for all specified jobs to finish before
  returning a list containing the last reported result for each
  job.   If \code{wait} is \code{FALSE} then \code{mccollect} merely
  checks for any results available at the moment and will not wait for
  jobs to finish.   If \code{jobs} is specified, jobs not listed there
  will not be affected or acted upon.

  Note: If \code{expr} uses low-level multicore functions such
  as \code{\link{sendMaster}} a single job can deliver results
  multiple times and it is the responsibility of the user to interpret
  them correctly.  \code{mccollect} will return \code{NULL} for a
  terminating job that has sent its results already after which the
  job is no longer available.

  The \code{mc.affinity} parameter can be used to try to restrict
  the child process to specific CPUs. The availability and the extent of
  this feature is system-dependent (e.g., some systems will only
  consider the CPU count, others will ignore it completely).
}
\value{
  \code{mcparallel} returns an object of the class \code{"parallelJob"}
  which inherits from \code{"childProcess"} (see the \sQuote{Value}
  section of the help for \code{\link{mcfork}}).  If argument
  \code{name} was supplied this will have an additional component
  \code{name}.

  \code{mccollect} returns any results that are available in a list.  The
  results will have the same order as the specified jobs.  If there are
  multiple jobs and a job has a name it will be used to name the
  result, otherwise its process ID will be used.  If none of the
  specified children are still running, it returns \code{NULL}.
}
\section{Random numbers}{
  If \code{mc.set.seed = FALSE}, the child process has the same initial
  random number generator (RNG) state as the current \R session.  If the
  RNG has been used (or \code{.Random.seed} was restored from a saved
  workspace), the child will start drawing random numbers at the same
  point as the current session.  If the RNG has not yet been used, the
  child will set a seed based on the time and process ID when it first
  uses the RNG: this is pretty much guaranteed to give a different
  random-number stream from the current session and any other child
  process.

  The behaviour with \code{mc.set.seed = TRUE} is different only if
  \code{\link{RNGkind}("L'Ecuyer-CMRG")} has been selected.  Then each
  time a child is forked it is given the next stream (see
  \code{\link{nextRNGStream}}).  So if you select that generator, set a
  seed and call \code{\link{mc.reset.stream}} just before the first use
  of \code{mcparallel} the results of simulations will be reproducible
  provided the same tasks are given to the first, second, \ldots{}
  forked process.
}

\note{
  Package \CRANpkg{multicore} also exported functions \code{collect} and
  \code{parallel}.  These names are easily masked (for example package
  \CRANpkg{lattice} also has a function \code{parallel}) and they are not
  supplied in this package.
}


\author{
  Simon Urbanek and R Core.

  Derived from the \CRANpkg{multicore} package (but with different
  handling of the RNG stream).
}
\seealso{
  \code{\link{pvec}}, \code{\link{mclapply}}
}
\examples{
p <- mcparallel(1:10)
q <- mcparallel(1:20)
# wait for both jobs to finish and collect all results
res <- mccollect(list(p, q))

p <- mcparallel(1:10)
mccollect(p, wait = FALSE, 10) # will retrieve the result (since it's fast)
mccollect(p, wait = FALSE)     # will signal the job as terminating
mccollect(p, wait = FALSE)     # there is no longer such a job

\dontshow{set.seed(123, "L'Ecuyer"); mc.reset.stream()}
# a naive parallel lapply can be created using mcparallel alone:
jobs <- lapply(1:10, function(x) mcparallel(rnorm(x), name = x))
mccollect(jobs)
}
\keyword{interface}
