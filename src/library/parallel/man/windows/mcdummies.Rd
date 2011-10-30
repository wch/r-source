% File src/library/parallel/man/windows/mcdummies.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2009-11 R Core Development Team
% Distributed under GPL 2 or later

\name{mclapply}
\alias{mclapply}
\alias{pvec}
\title{Serial versions of \code{mclapply} and \code{pvec}}
\description{
  These are simple serial versions of \code{mclapply} and \code{pvec}
  for Windows where forking is not available.
}
\usage{
mclapply(X, FUN, ..., mc.preschedule = TRUE, mc.set.seed = TRUE,
         mc.silent = FALSE, mc.cores = 1L,
         mc.cleanup = TRUE, mc.allow.recursive = TRUE)

pvec(v, FUN, ..., mc.set.seed = TRUE, mc.silent = FALSE,
     mc.cores = 1L, mc.cleanup = TRUE)
}
\arguments{
  \item{X}{a vector (atomic or list) or an expressions vector.  Other
    objects (including classed objects) will be coerced by
    \code{\link{as.list}}.}
  \item{v}{vector to operate on}
  \item{FUN}{the function to be applied to each element of \code{X} or \code{v}}
  \item{...}{optional arguments to \code{FUN}}
  \item{mc.preschedule, mc.set.seed, mc.silent, mc.cleanup, mc.allow.recursive}{
    Ignored on Windows.}
  \item{mc.cores}{The number of cores to use, i.e. at most how many
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

  For \code{pvec}, a vector of the same length as \code{v}.
}

\seealso{
  \code{\link{parLapply}}.
}

\keyword{interface}