% File src/library/base/man/gctorture.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{gctorture}
\title{Torture Garbage Collector}
\usage{
gctorture(on = TRUE)
gctorture2(step, wait = step, inhibit_release = FALSE)
}
\alias{gctorture}
\alias{gctorture2}
\arguments{
  \item{on}{logical; turning it on/off.}
  \item{step}{integer; run GC every \code{step} allocations; \code{step
      = 0} turns the GC torture off.}
  \item{wait}{integer; number of allocations to wait before starting
    GC torture.}
  \item{inhibit_release}{logical; do not release free objects for
    re-use: use with caution.}
}
\description{
  Provokes garbage collection on (nearly) every memory allocation.
  Intended to ferret out memory protection bugs.  Also makes \R run
  \emph{very} slowly, unfortunately.
}
\details{
  Calling \code{gctorture(TRUE)} instructs the memory manager to force a
  full GC on every allocation. \code{gctorture2} provides a more refined
  interface that allows the start of the GC torture to be deferred and
  also gives the option of running a GC only every \code{step}
  allocations.

  The third argument to \code{gctorture2} is only used if R has been
  configured with a strict write barrier enabled.  When this is
  the case all garbage collections are full collections, and the memory
  manager marks free nodes and enables checks in many situations that
  signal an error when a free node is used.  This can greatly help in
  isolating unprotected values in C code.  It does not detect the case
  where a node becomes free and is reallocated.  The
  \code{inhibit_release} argument can be used to prevent such
  reallocation.  This will cause memory to grow and should be used with
  caution and in conjunction with operating system facilities to monitor
  and limit process memory use.
}
\value{
  Previous value of first argument.
}
\author{Peter Dalgaard and Luke Tierney}
\keyword{environment}

