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
  \item{fun}{function or character string naming a function.}
  \item{expr}{expression to evaluate.}
  \item{seq}{vector to split.}
  \item{list}{character vector of variables to export.}
  \item{x}{array.}
  \item{...}{additional arguments to pass to standard function.}
  \item{MoreArgs}{additional arguments for \code{fun}.}
  \item{RECYCLE}{logical; if true shorter arguments are recycled.}
  \item{X}{A vector (atomic or list).}
  \item{FUN}{function or character string naming a function}
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
