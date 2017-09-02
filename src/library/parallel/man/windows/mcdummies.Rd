% File src/library/parallel/man/windows/mcdummies.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2009-2014 R Core Team
% Distributed under GPL 2 or later

\name{mclapply}
\alias{mclapply}
\alias{pvec}
\alias{mcmapply}
\alias{mcMap}

\title{Serial versions of \code{mclapply}, \code{mcmapply} and \code{pvec}}
\description{
  These are simple serial versions of \code{mclapply}, \code{mcmapply},
  \code{mcMap}  and \code{pvec} for Windows where forking is not available.
}
\usage{
mclapply(X, FUN, ..., mc.preschedule = TRUE, mc.set.seed = TRUE,
         mc.silent = FALSE, mc.cores = 1L,
         mc.cleanup = TRUE, mc.allow.recursive = TRUE, affinity.list = NULL)

mcmapply(FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE,
        mc.preschedule = TRUE, mc.set.seed = TRUE,
        mc.silent = FALSE, mc.cores = 1L, mc.cleanup = TRUE, affinity.list = NULL)

mcMap(f, ...)

pvec(v, FUN, ..., mc.set.seed = TRUE, mc.silent = FALSE,
     mc.cores = 1L, mc.cleanup = TRUE)
}
\arguments{
  \item{X}{a vector (atomic or list) or an expressions vector.  Other
    objects (including classed objects) will be coerced by
    \code{\link{as.list}}.}
  \item{v}{vector to operate on.}
  \item{FUN}{the function to be applied to each element of \code{X} or
     \code{v}, or in parallel to \code{...} .}
  \item{f}{the function to be applied in parallel to \code{\dots}.}
  \item{...}{For \code{mclapply} and \code{pvec}, optional arguments to
     \code{FUN}.  For \code{mcmapply} and \code{mcMap}, vector or list
     inputs: see \code{\link{mapply}}.}
  \item{MoreArgs, SIMPLIFY, USE.NAMES}{see \code{\link{mapply}}.}
  \item{mc.preschedule, mc.set.seed, mc.silent, mc.cleanup, mc.allow.recursive, affinity.list}{
    Ignored on Windows.}
  \item{mc.cores}{The number of cores to use, i.e.\sspace{}at most how many
    child processes will be run simultaneously.   Must be exactly 1 on
    Windows (which uses the master process).}
}

\details{
  \code{mclapply} calls \code{\link{lapply}} and \code{pvec} makes a
  single call \code{FUN(v, ...)}.   On Unix-alikes \code{mc.cores > 1}
  is allowed and uses parallel operations.
}

\value{
  For \code{mclapply}, a list of the same length as \code{X} and named
  by \code{X}.

  For \code{mcmapply}, a list, vector or array: see
  \code{\link{mapply}}.

  For \code{mcMap}, a list.

  For \code{pvec}, a vector of the same length as \code{v}.
}

\seealso{
  \code{\link{parLapply}}, \code{\link{clusterMap}}.
}

\keyword{interface}
