% File src/library/parallel/man/mcparallel.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2009-11 R Core Development Team
% Distributed under GPL 2 or later

\name{mcparallel}
\alias{mccollect}
\alias{mcparallel}
\title{Evaluate an \R Expression Asynchronously in a Separate Process}
\description{
  These functions are based on forking and so are not available on Windows.
  
  \code{mcparallel} starts a parallel \R process which evaluates the
  given expression.

  \code{mccollect} collects results from parallel processes.
}
\usage{
mcparallel(expr, name, mc.set.seed = FALSE, silent = FALSE)

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
\value{
  \code{mcparallel} returns an object of the class \code{"parallelJob"}
  which inherits from \code{"childProcess"} (see the \sQuote{Value}
  section of the help for \code{\link{mcfork}}).  If argument
  \code{name} was supplied this will have an additional component
  \code{name}.

  \code{mccollect} returns any results that are available in a list.  The
  results will have the same order as the specified jobs.  If there are
  multiple jobs and a job has a name it will be used to name the
  result, otherwise its process ID will be used.
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
}
\section{Random numbers}{
  If \code{mc.set.seed} is false, the child process has the same random
  number generator (RNG) state as the current \R session.  If the RNG
  has been used (or \code{.Random.seed} was restored from a saved
  workspace), the child will start drawing random numbers at the same
  point as the current session.  If the RNG has not yet been used, the
  child will set a seed based on the time and process ID when it first
  uses the RNG.
  
  The default behaviour is to set the seed in the child process at the
  time it is first used: this is pretty much guaranteed to give a
  different random-number stream from the current session and any other
  child process.  For an absolute guarantee, use
  \code{\link{RNGkind}("L'Ecuyer-CMRG")}: then each time a child is
  forked it is given the next stream (see \code{\link{nextRNGStream}}.
  So if you select that generator and set a seed just before the first
  use of \code{mcparallel} (including \emph{via} \code{\link{mclapply}})
  the results of simulations will be reproducible provided the same
  tasks are given to the first, second, \ldots{} forked process.
}
\seealso{
  \code{\link{sendMaster}}
}
\examples{
p <- mcparallel(1:10)
q <- mcparallel(1:20)
mccollect(list(p, q)) # wait for jobs to finish and collect all results

p <- mcparallel(1:10)
mccollect(p, wait=FALSE, 10) # will retrieve the result (since it's fast)
mccollect(p, wait=FALSE) # will signal the job as terminating
mccollect(p, wait=FALSE) # there is no such job

# a naive mcparallelized lapply can be created using parallel alone:
jobs <- lapply(1:10, function(x) mcparallel(rnorm(x), name = x))
mccollect(jobs)
}
\author{Simon Urbanek}
\keyword{interface}
