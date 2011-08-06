% File src/library/parallel/man/clusterApply.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2003-11 R Core Development Team
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
\alias{parlapply}

\title{Apply Operations using Clusters}
\description{
  These functions provide several ways to parallelize computations using
  a cluster.
}
\usage{
clusterSplit(cl, seq)
clusterCall(cl, fun, ...)
clusterApply(cl, x, fun, ...)
clusterApplyLB(cl, x, fun, ...)
clusterEvalQ(cl, expr)
clusterExport(cl, varlist)
clusterMap(cl, fun, ..., MoreArgs = NULL, RECYCLE = TRUE)

parLapply(cl, X, fun, ...)
parSapply(cl, X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE) 
parApply(cl, X, MARGIN, FUN, ...)
parRapply(cl, x, FUN, ...)
parCapply(cl, x, FUN, ...)
}
\arguments{
  \item{cl}{a cluster object, created by this package or by package \pkg{snow}.}
  \item{fun, FUN}{function or character string naming a function.}
  \item{expr}{expression to evaluate.}
  \item{seq}{vector to split.}
  \item{varlist}{character vector of names of objects to export.}
  \item{x}{a vector for \code{clusterApply} and \code{clusterApplyLB}, a
    matrix for \code{parRapply} and \code{parCapply}.}
  \item{...}{additional arguments to pass to \code{fun} or \code{FUN}:
    beware of partial matching to earlier arguments.}
  \item{MoreArgs}{additional arguments for \code{fun}.}
  \item{RECYCLE}{logical; if true shorter arguments are recycled.}
  \item{X}{A vector (atomic or list), an array for \code{parApply}.}
  \item{MARGIN}{vector specifying the dimensions to use.}
  \item{simplify}{logical; see \code{\link{sapply}}.}
  \item{USE.NAMES}{logical; see \code{\link{sapply}}.}
}
\details{
  \code{clusterCall} calls a function \code{fun} with identical
  arguments \code{...} on each cluster node.

  \code{clusterEvalQ} evaluates a literal expression on each cluster
  node.  It a parallel version of \code{\link{evalq}}, and is a convenience
  function defined in terms of \code{clusterCall}.

  \code{clusterApply} calls \code{fun} on the first cluster node with
  arguments \code{seq[[1]]} and \code{...}, on the second node with
  \code{seq[[2]]} and \code{...}, and so on, recycling nodes as needed.

  \code{clusterApplyLB} is a load balancing version of \code{clusterApply}.
  if the length \code{p} of \code{seq} is greater than the number of
  cluster nodes \code{n}, then the first \code{n} jobs are placed in
  order on the \code{n} nodes.  When the first job completes, the next
  job is placed on the available node; this continues until all jobs
  are complete.  Using \code{clusterApplyLB} can result in better
  cluster utilization than using \code{clusterApply}.  However,
  increased communication can reduce performance.  Furthermore, the
  node that executes a particular job is nondeterministic, which can
  complicate ensuring reproducibility in simulations.

  \code{clusterMap} is a multi-argument version of \code{clusterApply},
  analogous to \code{mapply}.  If \code{RECYCLE} is true shorter
  arguments are recycled; otherwise, the result length is the length of
  the shortest argument.  Cluster nodes are recycled if the length of
  the result is greater than the number of nodes.
  
  \code{clusterExport} assigns the global values on the master of the
  variables named in \code{list} to variables of the same names in the
  global environments of each node.

  \code{clusterSplit} splits \code{seq} into one consecutive piece for
  each cluster and returns the result as a list with length equal to the
  number of nodes.  Currently the pieces are chosen to be close
  to equal in length.
  
  \code{parLapply}, \code{parSapply}, and \code{parApply} are parallel
  versions of \code{lapply}, \code{sapply}, and \code{apply}.

  \code{parRapply} and \code{parCapply} are parallel row and column
  \code{apply} functions for a matrix \code{x}; they may be slightly
  more efficient than \code{parApply}.
  
  An error is signalled on the master if any of the workers produces an
  error.
}
\value{
  For \code{clusterCall}, code{clusterEvalQ} and \code{clusterSplit}, a
  list with one element per node.

  For \code{clusterApply} and \code{clusterApplyLB}, a list the same
  length as \code{seq}.

  \code{clusterMap} follows \code{\link{mapply}}.
  
  \code{clusterExport} returns nothing.

  \code{parLapply} returns a list the length of \code{X}.

  \code{parSapply} and \code{parApply} follow \code{\link{sapply}} and
  \code{\link{apply}} respectively.
}
\note{
  These functions are almost identical to those in package \pkg{snow}:
  they are copied here to avoid namespace scoping issues.

  One exception: \code{parLapply} has argument \code{X}
  not \code{x} for consistency with \code{\link{lapply}}.
}
\examples{\donttest{ ## access to ports might be denied
cl <- makeCluster(getOption("mc.cores", 2))

clusterApply(cl, 1:2, get("+"), 3)
xx <- 1
clusterExport(cl, "xx")
clusterCall(cl, function(y) xx + y, 2)

##FIXME: need example for clusterMap

parSapply(cl, 1:20, get("+"), 3)
stopCluster(cl)

## A bootstrapping example, which can be done in many ways:
cl <- makeCluster(getOption("mc.cores", 2))
clusterEvalQ(cl, {
  ## set up each worker.  Could also use clusterExport()
  library(boot)
  cd4.rg <- function(data, mle) MASS::mvrnorm(nrow(data), mle$m, mle$v)
  cd4.mle <- list(m = mean(cd4), v = var(cd4))
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
   cd4.mle <- list(m = mean(cd4), v = var(cd4))
   boot(cd4, corr, R = 500, sim = "parametric",
        ran.gen = cd4.rg, mle = cd4.mle)
}
cl <- makeCluster(mc <- getOption("mc.cores", 2))
## make this reproducible
clusterSetRNGStream(cl, 123)
cd4.boot <- do.call(c, parLapply(cl, seq_len(mc), run1))
boot.ci(cd4.boot,  type = c("norm", "basic", "perc"),
        conf = 0.9, h = atanh, hinv = tanh)
stopCluster(cl)
}}
