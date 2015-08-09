% File src/library/parallel/man/clusterApply.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2003-2014 R Core Team
% Distributed under GPL 2 or later

\name{clusterApply}

\alias{clusterApply}
\alias{clusterApplyLB}
\alias{clusterCall}
\alias{clusterEvalQ}
\alias{clusterExport}
\alias{clusterMap}
\alias{clusterSplit}
\alias{parApply}
\alias{parCapply}
\alias{parLapply}
\alias{parRapply}
\alias{parSapply}
\alias{parLapplyLB}
\alias{parSapplyLB}

\title{Apply Operations using Clusters}
\description{
  These functions provide several ways to parallelize computations using
  a cluster.
}
\usage{
clusterCall(cl = NULL, fun, ...)
clusterApply(cl = NULL, x, fun, ...)
clusterApplyLB(cl = NULL, x, fun, ...)
clusterEvalQ(cl = NULL, expr)
clusterExport(cl = NULL, varlist, envir = .GlobalEnv)
clusterMap(cl = NULL, fun, ..., MoreArgs = NULL, RECYCLE = TRUE,
           SIMPLIFY = FALSE, USE.NAMES = TRUE,
           .scheduling = c("static", "dynamic"))
clusterSplit(cl = NULL, seq)

parLapply(cl = NULL, X, fun, ...)
parSapply(cl = NULL, X, FUN, ..., simplify = TRUE,
          USE.NAMES = TRUE)
parApply(cl = NULL, X, MARGIN, FUN, ...)
parRapply(cl = NULL, x, FUN, ...)
parCapply(cl = NULL, x, FUN, ...)

parLapplyLB(cl = NULL, X, fun, ...)
parSapplyLB(cl = NULL, X, FUN, ..., simplify = TRUE,
            USE.NAMES = TRUE)
}
\arguments{
  \item{cl}{a cluster object, created by this package or by package
    \CRANpkg{snow}.  If \code{NULL}, use the registered default cluster.}
  \item{fun, FUN}{function or character string naming a function.}
  \item{expr}{expression to evaluate.}
  \item{seq}{vector to split.}
  \item{varlist}{character vector of names of objects to export.}
  \item{envir}{environment from which t export variables}
  \item{x}{a vector for \code{clusterApply} and \code{clusterApplyLB}, a
    matrix for \code{parRapply} and \code{parCapply}.}
  \item{...}{additional arguments to pass to \code{fun} or \code{FUN}:
    beware of partial matching to earlier arguments.}
  \item{MoreArgs}{additional arguments for \code{fun}.}
  \item{RECYCLE}{logical; if true shorter arguments are recycled.}
  \item{X}{A vector (atomic or list) for \code{parLapply} and
    \code{parSapply}, an array for \code{parApply}.}
  \item{MARGIN}{vector specifying the dimensions to use.}
  \item{simplify, USE.NAMES}{logical; see \code{\link{sapply}}.}
  \item{SIMPLIFY}{logical; see \code{\link{mapply}}.}
  \item{.scheduling}{should tasks be statically allocated to nodes or
    dynamic load-balancing used?}
}
\details{
  \code{clusterCall} calls a function \code{fun} with identical
  arguments \code{...} on each node.

  \code{clusterEvalQ} evaluates a literal expression on each cluster
  node.  It is a parallel version of \code{\link{evalq}}, and is a
  convenience function invoking \code{clusterCall}.

  \code{clusterApply} calls \code{fun} on the first node with
  arguments \code{seq[[1]]} and \code{...}, on the second node with
  \code{seq[[2]]} and \code{...}, and so on, recycling nodes as needed.

  \code{clusterApplyLB} is a load balancing version of
  \code{clusterApply}.  If the length \code{p} of \code{seq} is not
  greater than the number of nodes \code{n}, then a job is sent to
  \code{p} nodes.  Otherwise the first \code{n} jobs are placed in order
  on the \code{n} nodes.  When the first job completes, the next job is
  placed on the node that has become free; this continues until all jobs
  are complete.  Using \code{clusterApplyLB} can result in better
  cluster utilization than using \code{clusterApply}, but increased
  communication can reduce performance.  Furthermore, the node that
  executes a particular job is non-deterministic.

  \code{clusterMap} is a multi-argument version of \code{clusterApply},
  analogous to \code{\link{mapply}} and \code{\link{Map}}.  If
  \code{RECYCLE} is true shorter arguments are recycled (and either none
  or all must be of length zero); otherwise, the result length is the
  length of the shortest argument.  Nodes are recycled if the length of
  the result is greater than the number of nodes.  (\code{mapply} always
  uses \code{RECYCLE = TRUE}, and has argument \code{SIMPLIFY = TRUE}.
  \code{Map} always uses \code{RECYCLE = TRUE}.)

  \code{clusterExport} assigns the values on the master \R process of
  the variables named in \code{varlist} to variables of the same names
  in the global environment (aka \sQuote{workspace}) of each node.  The
  environment on the master from which variables are exported defaults
  to the global environment.

  \code{clusterSplit} splits \code{seq} into a consecutive piece for
  each cluster and returns the result as a list with length equal to the
  number of nodes.  Currently the pieces are chosen to be close
  to equal in length: the computation is done on the master.

  \code{parLapply}, \code{parSapply}, and \code{parApply} are parallel
  versions of \code{lapply}, \code{sapply} and \code{apply}.
  \code{parLapplyLB}, \code{parSapplyLB} are load-balancing versions,
  intended for use when applying \code{FUN} to different elements of
  \code{X} takes quite variable amounts of time, and either the function
  is deterministic or reproducible results are not required.

  \code{parRapply} and \code{parCapply} are parallel row and column
  \code{apply} functions for a matrix \code{x}; they may be slightly
  more efficient than \code{parApply} but do less post-processing of the
  result.
}
\value{
  For \code{clusterCall}, \code{clusterEvalQ} and \code{clusterSplit}, a
  list with one element per node.

  For \code{clusterApply} and \code{clusterApplyLB}, a list the same
  length as \code{seq}.

  \code{clusterMap} follows \code{\link{mapply}}.

  \code{clusterExport} returns nothing.

  \code{parLapply} returns a list the length of \code{X}.

  \code{parSapply} and \code{parApply} follow \code{\link{sapply}} and
  \code{\link{apply}} respectively.

  \code{parRapply} and \code{parCapply} always return a vector.  If
  \code{FUN} always returns a scalar result this will be of length the
  number of rows or columns: otherwise it will be the concatenation of
  the returned values.

  An error is signalled on the master if any of the workers produces an
  error.
}
\note{
  These functions are almost identical to those in package \CRANpkg{snow}.

  Two exceptions: \code{parLapply} has argument \code{X}
  not \code{x} for consistency with \code{\link{lapply}}, and
  \code{parSapply} has been updated to match \code{\link{sapply}}.
}
\author{
  Luke Tierney and R Core.

  Derived from the \CRANpkg{snow} package.
}
% donttest, as access to ports might be denied.  Tested in the 'tests' directory
\examples{\donttest{
## Use option cl.cores to choose an appropriate cluster size.
cl <- makeCluster(getOption("cl.cores", 2))

clusterApply(cl, 1:2, get("+"), 3)
xx <- 1
clusterExport(cl, "xx")
clusterCall(cl, function(y) xx + y, 2)

## Use clusterMap like an mapply example
clusterMap(cl, function(x, y) seq_len(x) + y,
          c(a =  1, b = 2, c = 3), c(A = 10, B = 0, C = -10))


parSapply(cl, 1:20, get("+"), 3)

## A bootstrapping example, which can be done in many ways:
clusterEvalQ(cl, {
  ## set up each worker.  Could also use clusterExport()
  library(boot)
  cd4.rg <- function(data, mle) MASS::mvrnorm(nrow(data), mle$m, mle$v)
  cd4.mle <- list(m = colMeans(cd4), v = var(cd4))
  NULL
})
res <- clusterEvalQ(cl, boot(cd4, corr, R = 100,
                    sim = "parametric", ran.gen = cd4.rg, mle = cd4.mle))
library(boot)
cd4.boot <- do.call(c, res)
boot.ci(cd4.boot,  type = c("norm", "basic", "perc"),
        conf = 0.9, h = atanh, hinv = tanh)
stopCluster(cl)

## or
library(boot)
run1 <- function(...) {
   library(boot)
   cd4.rg <- function(data, mle) MASS::mvrnorm(nrow(data), mle$m, mle$v)
   cd4.mle <- list(m = colMeans(cd4), v = var(cd4))
   boot(cd4, corr, R = 500, sim = "parametric",
        ran.gen = cd4.rg, mle = cd4.mle)
}
cl <- makeCluster(mc <- getOption("cl.cores", 2))
## to make this reproducible
clusterSetRNGStream(cl, 123)
cd4.boot <- do.call(c, parLapply(cl, seq_len(mc), run1))
boot.ci(cd4.boot,  type = c("norm", "basic", "perc"),
        conf = 0.9, h = atanh, hinv = tanh)
stopCluster(cl)
}}
