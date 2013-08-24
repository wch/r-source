% File src/library/parallel/man/mclapply.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2009-13 R Core Team
% Distributed under GPL 2 or later

\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

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
         mc.cleanup = TRUE, mc.allow.recursive = TRUE)

mcmapply(FUN, ...,
         MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE,
         mc.preschedule = TRUE, mc.set.seed = TRUE,
         mc.silent = FALSE, mc.cores = getOption("mc.cores", 2L),
         mc.cleanup = TRUE)

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
  \item{mc.cores}{The number of cores to use, i.e. at most how many
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
}

\details{
  \code{mclapply} is a parallelized version of \code{\link{lapply}},
  provided \code{mc.cores > 1}: for \code{mc.cores == 1} it simply calls
  \code{lapply}.

  By default (\code{mc.preschedule = TRUE}) the input \code{X} is split
  into as many parts as there are cores (currently the values are spread
  across the cores sequentially, i.e. first value to core 1, second to
  core 2, ... (core + 1)-th value to core 1 etc.) and then one process
  is forked to each core and the results are collected.

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
  descriptors (fork will fail with error \code{"unable to create a
  pipe"}).
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
  involved in the failure, even if not all of them failed.
}

\section{Warning}{
  It is \emph{strongly discouraged} to use these functions in GUI or
  embedded environments, because it leads to several processes sharing
  the same GUI which will likely cause chaos (and possibly
  crashes).  Child processes should never use on-screen graphics
  devices.

  Some precautions have been taken to make this usable in
  \command{R.app} on OS X, but users of third-party front-ends
  should consult their documentation.

  Note that \pkg{tcltk} counts as a GUI for these purposes since
  \command{Tcl} runs an event loop.  That event loop
  is inhibited in a child process but there could still be problems with
  Tk graphical connections.
}

\author{
  Simon Urbanek and R Core.

  Derived from the \CRANpkg{multicore} package.
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
}}
\keyword{interface}
