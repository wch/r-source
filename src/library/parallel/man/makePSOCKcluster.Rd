% File src/library/parallel/man/makePSOCKcluster.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2011 R Core Development Team
% Distributed under GPL 2 or later

\name{makePSOCKcluster}
\alias{makePSOCKcluster}
\alias{makeCluster}
\title{
  Create a Parallel Socket Cluster
}
\description{
  Creates a set of copies of \R running in parallel and communicating
  over sockets.
}
\usage{
makeCluster(names, ...)
makePSOCKcluster(names, ...)
}
\arguments{
  \item{names}{Either a character vector of host names on whch to run
    the worker copies on \R, or a non-negative integer (in which case
    that number of copies is run on \samp{localhost}).}
  \item{\dots}{Options to be passed to the function spawning the workers.
    See \sQuote{details}.}
}
\details{
  This function is very similar to \code{makeSOCKcluster} in package
  \pkg{snow}).  It runs \command{Rscript} on the specified host(s) to set
  up a worker process which listens on a socket for expressions to
  evaluate, and returns the results (as serialized objects).

  The workers are most often running on the same host as the master,
  when no options need be set.

  Several options are supported:
  \describe{
    \item{\code{master}}{The host name of the master.}
    \item{\code{port}}{The port number for the socket connection,
      default taken from the environment variable \env{R_PARALLEL_PORT},
      then \code{10187}.}
    \item{\code{timeout}}{The timeout in seconds for that port.  Default
      1 year.}
    \item{\code{outfile}}{Where to send the stdout from the workers.
      Defaults to \file{/dev/null} (\file{nul:} on Windows).}
    \item{\code{homogeneous}}{Logical.  Are all the hosts running
      identical setups, so \command{Rscript} can be launched using its
      full path on the master?  Otherwise it has to be in the default
      path on the other hosts.}
    \item{\code{rscript}}{The path to \command{Rscript} on the workers.
      Defaults to the full path on the master.}
    \item{\code{rshcmd}}{The command to be run on the master to launch a
      process on another host.  Defaults to \command{ssh}.}
    \item{\code{user}}{The user name to be used when communicating with
      another host.}
    \item{\code{manual}}{Logical.  If true the workers will need to be
    run manually.}
  }
}
\value{
  An object of class \code{c("SOCKcluster", "cluster")}.
}

\seealso{
  \code{makeCluster} in package \pkg{snow}.
}
