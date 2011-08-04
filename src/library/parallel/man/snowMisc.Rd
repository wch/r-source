% File src/library/parallel/man/snowMisc.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2011 R Core Development Team
% Distributed under GPL 2 or later

\name{snowMisc}

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
\alias{stopCluster}

\title{Functions Borrowed from Package SNOW}
\description{
  Functions borrowed \emph{pro tem} from package \pkg{snow}.
}
\usage{
clusterSplit(cl, seq)
clusterCall(cl, fun, ...)
clusterApply(cl, x, fun, ...)
clusterApplyLB(cl, x, fun, ...)
clusterEvalQ(cl, expr)
clusterExport(cl, list)
clusterMap(cl, fun, ..., MoreArgs = NULL, RECYCLE = TRUE)

parLapply(cl, X, fun, ...)
parSapply(cl, X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE) 
parApply(cl, X, MARGIN, FUN, ...)
parRapply(cl, x, fun, ...)
parCapply(cl, x, fun, ...)

stopCluster(cl)
}
\arguments{
  \item{cl}{cluster object.}
  \item{fun, FUN}{function or character string naming a function.}
  \item{expr}{expression to evaluate.}
  \item{seq}{vector to split.}
  \item{list}{character vector of names of objects to export.}
  \item{x}{a matrix or array.}
  \item{...}{additional arguments to pass to standard function.}
  \item{MoreArgs}{additional arguments for \code{fun}.}
  \item{RECYCLE}{logical; if true shorter arguments are recycled.}
  \item{X}{A vector (atomic or list).}
  \item{MARGIN}{vector specifying the dimensions to use.}
  \item{simplify}{logical; see \code{\link{sapply}}.}
  \item{USE.NAMES}{logical; see \code{\link{sapply}}.}
}
\details{
  These functions are identical to those in package \pkg{snow}: they are
  copied here to avoid namespace scoping issues.

  There is one small exception: \code{parLapply} has argument \code{X}
  not \code{x} for consistency with \code{\link{apply}}.
  
  \code{cl} should be a cluster created by this package or by package
  \pkg{snow}.
}
\seealso{
  The snow help pages \link[snow]{snow-cluster} and
  \link[snow]{snow-parallel}.
}
\examples{\donttest{ ## access to ports might be denied
cl <- makeCluster(getOption("mc.cores", 2))

clusterApply(cl, 1:2, get("+"), 3)
xx <- 1
clusterExport(cl, "xx")
clusterCall(cl, function(y) xx + y, 2)

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
