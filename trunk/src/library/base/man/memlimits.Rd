% File src/library/base/man/memlimits.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{memlimits}
\alias{mem.maxVSize}
\alias{mem.maxNSize}
\title{Query and Set Heap Size Limits}
\description{
  Query and set the maximal size of the vector heap and the maximal
  number of heap nodes for the current \R process.
}
\usage{
mem.maxVSize(vsize = 0)
mem.maxNSize(nsize = 0)
}
\arguments{
  \item{vsize}{numeric; new size limit in Mb.}
  \item{nsize}{numeric; new maximal node number.}
}
\details{
  New Limits lower than current usage are ignored.
  Specifying a size of \code{Inf} sets the limit to the maximal possible
  value for the platform.

  The default maximal values are unlimited on most platforms, but can be
  adjusted using environment variables as described in
  \code{\link{Memory}}. On macOS a lower default vector heap limit is
  used to protect against the \R process being killed when macOS
  over-commits memory.

  Adjusting the maximal number of nodes is rarely necessary. Adjusting
  the vector heap size limit can be useful on macOS in particular but
  should be done with caution.
}
\value{
  The current or new value, in Mb for \code{mem.maxVSize}. \code{Inf} is
  returned if the current value is unlimited.
}

\seealso{
  \code{\link{Memory}}.
}
\keyword{utilities}
