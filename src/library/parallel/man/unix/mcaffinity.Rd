% File src/library/parallel/man/unix/mcaffinity.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2009-2014 R Core Team
% Distributed under GPL 2 or later

\name{mcaffinity}
\alias{mcaffinity}

\title{Get or Set CPU Affinity Mask of the Current Process}
\description{
  \code{mcaffinity} retrieves or sets the CPU affinity mask of the
  current process, i.e., the set of CPUs the process is allowed to be
  run on. (CPU here means logical CPU which can be CPU, core or
  hyperthread unit.)
}
\usage{
mcaffinity(affinity = NULL)
}
\arguments{
  \item{affinity}{specification of the CPUs to lock this process to
    (numeric vector) or \code{NULL} if no change is requested}
}
\details{
  \code{mcaffinity} can be used to obtain (\code{affinity = NULL})
  or set the CPU affinity mask of the current process. The affinity mask
  is a list of integer CPU identifiers (starting from 1) that this
  process is allowed to run on. Not all systems provide user access to
  the process CPU affinity, in cases where no support is present at all
  \code{mcaffinity()} will return \code{NULL}. Some systems may take
  into account only the number of CPUs present in the mask.

  Typically, it is legal to specify larger set than the number of
  logical CPUs (but at most as many as the OS can handle) and the system
  will return back the actually present set.
}
\value{
  \code{NULL} if CPU affinity is not supported by the system or an
  integer vector with the set of CPUs in the active affinity mask for
  this process (this may be different than \code{affinity}).
}
\author{
  Simon Urbanek.
}
\seealso{
  \code{\link{mcparallel}}
}
\keyword{interface}
