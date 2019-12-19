% File src/library/parallel/man/mclapply.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2009-2019 R Core Team
% Distributed under GPL 2 or later

\name{mclapply}
\alias{mclapply}
\alias{mcmapply}
\alias{mcMap}

\title{Parallel Versions of \code{lapply} and \code{mapply} using Forking}
\description{
  \code{mclapply} is a parallelized version of \code{\link{lapply}},
  it returns a list of the same length as \code{X}, each element of
  which is the result of applying \code{FUN} to the corresponding
  element of \code{X}.

  It relies on forking and hence is not available on Windows unless
  \code{mc.cores = 1}.

  \code{mcmapply} is a parallelized version of \code{\link{mapply}}, and
  \code{mcMap} corresponds to \code{\link{Map}}.
}
\usage{
mclapply(X, FUN, ...,
         mc.preschedule = TRUE, mc.set.seed = TRUE,
         mc.silent = FALSE, mc.cores = getOption("mc.cores", 2L),
         mc.cleanup = TRUE, mc.allow.recursive = TRUE, affinity.list = NULL)

mcmapply(FUN, ...,
         MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE,
         mc.preschedule = TRUE, mc.set.seed = TRUE,
         mc.silent = FALSE, mc.cores = getOption("mc.cores", 2L),
         mc.cleanup = TRUE, affinity.list = NULL)

mcMap(f, ...)
}
\arguments{
  \item{X}{a vector (atomic or list) or an expressions vector.  Other
    objects (including classed objects) will be coerced by
    \code{\link{as.list}}.}
  \item{FUN}{the function to be applied to (\code{mclapply}) each
    element of \code{X} or (\code{mcmapply}) in parallel to \code{\dots}.}
  \item{f}{the function to be applied in parallel to \code{\dots}.}
  \item{...}{For \code{mclapply}, optional arguments to \code{FUN}.
    For \code{mcmapply} and \code{mcMap}, vector or list inputs: see
    \code{\link{mapply}}.}
  \item{MoreArgs, SIMPLIFY, USE.NAMES}{see \code{\link{mapply}}.}
  \item{mc.preschedule}{if set to \code{TRUE} then the computation is
    first divided to (at most) as many jobs are there are cores and then
    the jobs are started, each job possibly covering more than one
    value.  If set to \code{FALSE} then one job is forked for each value
    of \code{X}.  The former is better for short computations or large
    number of values in \code{X}, the latter is better for jobs that
    have high variance of completion time and not too many values of
    \code{X} compared to \code{mc.cores}.}
  \item{mc.set.seed}{See \code{\link{mcparallel}}.}
  \item{mc.silent}{if set to \code{TRUE} then all output on
    \file{stdout} will be suppressed for all parallel processes forked
    (\file{stderr} is not affected).}
  \item{mc.cores}{The number of cores to use, i.e.\sspace{}at most how many
    child processes will be run simultaneously.   The option is
    initialized from environment variable \env{MC_CORES} if set.  Must
    be at least one, and parallelization requires at least two cores.}
  \item{mc.cleanup}{if set to \code{TRUE} then all children that have
    been forked by this function will be killed (by sending
    \code{SIGTERM}) before this function returns.  Under normal
    circumstances \code{mclapply} waits for the children to deliver
    results, so this option usually has only effect when \code{mclapply}
    is interrupted. If set to \code{FALSE} then child processes are
    collected, but not forcefully terminated.  As a special case this
    argument can be set to the number of the signal that should be used
    to kill the children instead of \code{SIGTERM}.}
  \item{mc.allow.recursive}{Unless true, calling \code{mclapply} in a
    child process will use the child and not fork again.}
  \item{affinity.list}{a vector (atomic or list) containing the CPU
    affinity mask for each element of \code{X}.  The CPU affinity mask
    describes on which CPU (core or hyperthread unit) a given item is
    allowed to run, see \code{\link{mcaffinity}}.  To use this parameter
    prescheduling has to be deactivated (\code{mc.preschedule = FALSE}).}
}

\details{
  \code{mclapply} is a parallelized version of \code{\link{lapply}},
  provided \code{mc.cores > 1}: for \code{mc.cores == 1} (and the
  \code{affinity.list} is \code{NULL}) it simply calls \code{lapply}.

  By default (\code{mc.preschedule = TRUE}) the input \code{X} is split
  into as many parts as there are cores (currently the values are spread
  across the cores sequentially, i.e.\sspace{}first value to core 1,
  second to core 2, \ldots (core + 1)-th value to core 1 etc.) and then
  one process is forked to each core and the results are collected.

  Without prescheduling, a separate job is forked for each value of
  \code{X}.  To ensure that no more than \code{mc.cores} jobs are
  running at once, once that number has been forked the master process
  waits for a child to complete before the next fork.

  Due to the parallel nature of the execution random numbers are not
  sequential (in the random number sequence) as they would be when using
  \code{lapply}.  They are sequential for each forked process, but not
  all jobs as a whole.  See \code{\link{mcparallel}} or the package's
  vignette for ways to make the results reproducible with
  \code{mc.preschedule = TRUE}.

  Note: the number of file descriptors (and processes) is usually
  limited by the operating system, so you may have trouble using more
  than 100 cores or so (see \code{ulimit -n} or similar in your OS
  documentation) unless you raise the limit of permissible open file
  descriptors (fork will fail with error \code{"unable to create a pipe"}).

  Prior to \R 3.4.0 and on a 32-bit platform, the \link{serialize}d
  result from each forked process is limited to \eqn{2^{31} - 1}{2^31 - 1}
  bytes.  (Returning very large results via serialization is
  inefficient and should be avoided.)

  \code{affinity.list} can be used to run elements of \code{X} on
  specific CPUs.  This can be helpful, if elements of \code{X} have a
  high variance of completion time or if the hardware architecture is
  heterogeneous.  It also enables the development of scheduling
  strategies for optimizing the overall runtime of parallel jobs.  If
  \code{affinity.list} is set, the \code{mc.core} parameter is replaced
  with the number of CPU ids used in the affinity masks.
}

\value{
  For \code{mclapply}, a list of the same length as \code{X} and named
  by \code{X}.

  For \code{mcmapply}, a list, vector or array: see
  \code{\link{mapply}}.

  For \code{mcMap}, a list.

  Each forked process runs its job inside \code{try(..., silent = TRUE)}
  so if errors occur they will be stored as class \code{"try-error"}
  objects in the return value and a warning will be given.  Note that
  the job will typically involve more than one value of \code{X} and
  hence a \code{"try-error"} object will be returned for all the values
  involved in the failure, even if not all of them failed. If any forked
  process is killed or fails to deliver a result for any reason, values
  involved in the failure will be \code{NULL}. To allow detection of such
  errors, \code{FUN} should not return \code{NULL}. As of \R 4.0, the
  return value of \code{mcmapply} is always a list when it needs to contain
  \code{"try-error"} objects (\code{SIMPLIFY} is overridden to \code{FALSE}).
}

\section{Warning}{
  It is \emph{strongly discouraged} to use these functions in GUI or
  embedded environments, because it leads to several processes sharing
  the same GUI which will likely cause chaos (and possibly
  crashes).  Child processes should never use on-screen graphics
  devices.

  Some precautions have been taken to make this usable in
  \command{R.app} on macOS, but users of third-party front-ends
  should consult their documentation.

  Note that \pkg{tcltk} counts as a GUI for these purposes since
  \command{Tcl} runs an event loop.  That event loop
  is inhibited in a child process but there could still be problems with
  Tk graphical connections.

  It is \emph{strongly discouraged} to use these functions with
  multi-threaded libraries or packages (see \code{\link{mcfork}} for more
  details).  If in doubt, it is safer to use a non-FORK cluster (see
  \code{\link{makeCluster}}, \code{\link{clusterApply}}).
}

\author{
  Simon Urbanek and R Core.
  The \code{affinity.list} feature by Helena Kotthaus and Andreas Lang,
  TU Dortmund.
  Derived from the \pkg{multicore} package formerly on \acronym{CRAN}.
}
\seealso{
  \code{\link{mcparallel}}, \code{\link{pvec}},
  \code{\link{parLapply}}, \code{\link{clusterMap}}.

  \code{\link{simplify2array}} for results like \code{\link{sapply}}.
}
\examples{\donttest{
simplify2array(mclapply(rep(4, 5), rnorm))
# use the same random numbers for all values
set.seed(1)
simplify2array(mclapply(rep(4, 5), rnorm, mc.preschedule = FALSE,
                        mc.set.seed = FALSE))

## Contrast this with the examples for clusterCall
library(boot)
cd4.rg <- function(data, mle) MASS::mvrnorm(nrow(data), mle$m, mle$v)
cd4.mle <- list(m = colMeans(cd4), v = var(cd4))
mc <- getOption("mc.cores", 2)
run1 <- function(...) boot(cd4, corr, R = 500, sim = "parametric",
                           ran.gen = cd4.rg, mle = cd4.mle)
## To make this reproducible:
set.seed(123, "L'Ecuyer")
res <- mclapply(seq_len(mc), run1)
cd4.boot <- do.call(c, res)
boot.ci(cd4.boot,  type = c("norm", "basic", "perc"),
        conf = 0.9, h = atanh, hinv = tanh)

## Usage of the affinity.list parameter
A <- runif(2500000,0,100)
B <- runif(2500000,0,100)
C <- runif(5000000,0,100)
first <- function(i) head(sort(i), n = 1)

# Restict all elements of X to run on CPU 1 and 2
affL <- list(c(1,2), c(1,2), c(1,2))
mclapply(list(A, A, A), first, mc.preschedule = FALSE, affinity.list = affL)


# Completion times are assumed to have a high variance
# To optimize the overall execution time elements of X are scheduled to suitable CPUs
# Assuming that the runtime for C is as long as the runtime of A plus B
# mapping: A to 1 , B to 1, C to 2
X <- list(A, B, C)
affL <- c(1, 1, 2)
mclapply(X, first, mc.preschedule = FALSE, affinity.list = affL)

}}
\keyword{interface}
