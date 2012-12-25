% File src/library/parallel/man/splitIndices.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2003-11 R Core Team
% Distributed under GPL 2 or later

\name{splitIndices}
\alias{splitIndices}

\title{Divide Tasks for Distribution in a Cluster}
\description{
  This divides up \code{1:nx} into \code{ncl} lists of approximately
  equal size, as a way to allocate tasks to nodes in a cluster.

  It is mainly for internal use, but some package authors have found it useful.
}
\usage{
splitIndices(nx, ncl)
}
\arguments{
  \item{nx}{Number of tasks.}
  \item{ncl}{Number of cluster nodes.}
}
\value{
  A list of length \code{ncl}, each element being an integer vector.
}
\examples{
splitIndices(20, 3)
}
\keyword{ utility }
