% File src/library/parallel/man/mclapply.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2009-11 R Core Development Team
% Distributed under GPL 2 or later

\name{mclapply}
\alias{mclapply}
\title{Parallel Version of \code{lapply}}
\description{
  \code{mclapply} is a parallelized version of \code{\link{lapply}},
  it returns a list of the same length as \code{X}, each element of
  which is the result of applying \code{FUN} to the corresponding
  element of \code{X}.
}
\usage{
mclapply(X, FUN, ..., mc.preschedule = TRUE, mc.set.seed = TRUE,
         mc.silent = FALSE, mc.cores = getOption("mc.cores", 2L),
         mc.cleanup = TRUE)
}
\arguments{
  \item{X}{a vector (atomic or list) or an expressions vector.  Other
    objects (including classed objects) will be coerced by
    \code{\link{as.list}}.}
  \item{FUN}{the function to be applied to each element of \code{X}}
  \item{...}{optional arguments to \code{FUN}}
  \item{mc.preschedule}{if set to \code{TRUE} then the computation is
    first divided to (at most) as many jobs are there are cores and then
    the jobs are started, each job possibly covering more than one
    value. If set to \code{FALSE} then one job is forked for each value
    of \code{X} sequentially (if used with \code{mc.set.seed=FALSE} then
    random number sequences will be identical for all values). The former
    is better for short computations or large number of values in
    \code{X}, the latter is better for jobs that have high variance of
    completion time and not too many values of \code{X}.}
  \item{mc.set.seed}{See \code{\link{mcparallel}}.}
  \item{mc.silent}{if set to \code{TRUE} then all output on stdout will be
    suppressed for all parallel processes forked (stderr is not affected).}
  \item{mc.cores}{The number of cores to use, i.e. how many processes
    will be forked (at most)}
  \item{mc.cleanup}{if set to \code{TRUE} then all children that have been
    forked by this function will be killed (by sending \code{SIGTERM})
    before this function returns. Under normal circumstances
    \code{mclapply} waits for the children to deliver results, so this
    option usually has only effect when \code{mclapply} is interrupted.
    If set to \code{FALSE} then child processes are collected, but not
    forcefully terminated. As a special case this argument can be set to
    the signal value that should be used to kill the children instead of
    \code{SIGTERM}.}
}
\details{
  \code{mclapply} is a parallelized version of \code{lapply}.  By
  default (\code{mc.preschedule = TRUE}) the input vector/list \code{X}
  is split into as many parts as there are cores (currently the values
  are spread across the cores sequentially, i.e. first value to core
  1, second to core 2, ... (core + 1)-th value to core 1 etc.) and
  then one process is forked to each core and the results are
  collected.

  Due to the parallel nature of the execution random numbers are not
  sequential (in the random number sequence) as they would be in
  \code{lapply}.  They are sequential for each forked process, but not
  all jobs as a whole.

  In addition, each process is running the job inside
  \code{try(..., silent=TRUE)} so if errors occur they will be stored as
  class \code{"try-error"} objects in the list.
  
  Note: the number of file descriptors is usually limited by the
  operating system, so you may have trouble using more than 100 cores
  or so (see \code{ulimit -n} or similar in your OS documentation)
  unless you raise the limit of permissible open file descriptors
  (fork will fail with error \code{"unable to create a pipe"}).
}
\value{
  A list.
}
\seealso{
  \code{\link{mcparallel}}, \code{\link{pvec}}
  
  \code{\link{simplify2array}} for results like \code{\link{sapply}}.
}
\examples{\donttest{
simplify2array(mclapply(rep(4, 5), rnorm))
# use the same random numbers for all values
set.seed(1)
simplify2array(mclapply(rep(4, 5), rnorm, mc.preschedule = FALSE,
                mc.set.seed = FALSE))
}}
\author{Simon Urbanek}
\keyword{interface}
