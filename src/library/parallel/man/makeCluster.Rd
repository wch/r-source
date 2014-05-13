% File src/library/parallel/man/makePSOCKcluster.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2003-2013 R Core Team
% Distributed under GPL 2 or later

\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{makeCluster}
\alias{makeCluster}
\alias{makePSOCKcluster}
\alias{makeForkCluster}
\alias{stopCluster}
\alias{setDefaultCluster}
\title{
  Create a Parallel Socket Cluster
}
\description{
  Creates a set of copies of \R running in parallel and communicating
  over sockets.
}
\usage{
makeCluster(spec, type, ...)
makePSOCKcluster(names, ...)
makeForkCluster(nnodes = getOption("mc.cores", 2L), ...)

stopCluster(cl = NULL)

setDefaultCluster(cl = NULL)
}
\arguments{
  \item{spec}{A specification appropriate to the type of cluster.}
  \item{names}{Either a character vector of host names on which to run
    the worker copies of \R, or a positive integer (in which case
    that number of copies is run on \samp{localhost}).}
  \item{nnodes}{The number of nodes to be forked.}
  \item{type}{One of the supported types: see \sQuote{Details}.}
  \item{\dots}{Options to be passed to the function spawning the workers.
    See \sQuote{Details}.}
  \item{cl}{an object of class \code{"cluster"}.}
}
\details{
  \code{makeCluster} creates a cluster of one of the supported types.
  The default type, \code{"PSOCK"}, calls \code{makePSOCKcluster}.  Type
  \code{"FORK"} calls \code{makeForkCluster}.  Other types are passed to
  package \CRANpkg{snow}.

  \code{makePSOCKcluster} is very similar to \code{makeSOCKcluster} in
  package \CRANpkg{snow}.  It runs \command{Rscript} on the specified
  host(s) to set up a worker process which listens on a socket for
  expressions to evaluate, and returns the results (as serialized
  objects).

  \code{makeForkCluster} is merely a stub on Windows.  On Unix-alike
  platforms it creates the worker process by forking.

  The workers are most often running on the same host as the master,
  when no options need be set.

  Several options are supported (mainly for \code{makePSOCKcluster}):
  \describe{
    \item{\code{master}}{The host name of the master, as known to the
      workers.  This may not be the same as it is known to the master,
      and on private subnets it may be necessary to specify this as a
      numeric IP address.  For example, OS X is likely to detect a
      machine as \samp{somename.local}, a name known only to itself.}
    \item{\code{port}}{The port number for the socket connection,
      default taken from the environment variable \env{R_PARALLEL_PORT},
      then a randomly chosen port in the range \code{11000:11999}.}
    \item{\code{timeout}}{The timeout in seconds for that port.  Default
      30 days (and the POSIX standard only requires values up to 31 days
      to be supported).}
    \item{\code{outfile}}{Where to direct the \code{\link{stdout}} and
      \code{\link{stderr}} connection output from the workers.
      \code{""} indicates no redirection (which may only be useful for
      workers on the local machine).
      Defaults to \file{/dev/null} (\file{nul:} on Windows).  The other
      possibility is a file path on the worker's host.
      Files will be opened in append mode, as all workers log to the
      same file.}
    \item{\code{homogeneous}}{Logical.  Are all the hosts running
      identical setups, so \command{Rscript} can be launched using
      the same path on each?  Otherwise \command{Rscript} has to be in
      the default path on the workers.}
    \item{\code{rscript}}{The path to \command{Rscript} on the workers,
      used if \code{homogeneous} is true. Defaults to the full path on
      the master.}
    \item{\code{rscript_args}}{Character vector of additional
      arguments for \command{Rscript} such as \option{--no-environ}.}
    \item{\code{renice}}{A numerical \sQuote{niceness} to set for the
      worker processes, e.g.{} \code{15} for a low priority.
      OS-dependent: see \code{\link{psnice}} for details.}
    \item{\code{rshcmd}}{The command to be run on the master to launch a
      process on another host.  Defaults to \command{ssh}.}
    \item{\code{user}}{The user name to be used when communicating with
      another host.}
    \item{\code{manual}}{Logical.  If true the workers will need to be
      run manually.}
    \item{\code{methods}}{Logical.  If true (default) the workers will
      load the \pkg{methods} package: not loading it saves ca 30\% of the
      startup CPU time of the cluster.}
    \item{\code{useXDR}}{Logical. If true (default) serialization will
      use XDR: where large amounts of data are to be transferred and
      all the nodes are little-endian, communication may be
      substantially faster if this is set to false.}
  }

  Function \code{makeForkCluster} creates a socket cluster by forking
  (and hence is not available on Windows).  It supports options
  \code{port}, \code{timeout} and \code{outfile}, and always uses
  \code{useXDR = FALSE}.

  It is good practice to shut down the workers by calling
  \code{\link{stopCluster}}: however the workers will terminate
  themselves once the socket on which they are listening for commands
  becomes unavailable, which it should if the master \R session is
  completed (or its process dies).

  Function \code{setDefaultCluster} registers a cluster as the default one
  for the current session.  Using \code{setDefaultCluster(NULL)} removes
  the registered cluster, as does stopping that cluster.
}

\value{
  An object of class \code{c("SOCKcluster", "cluster")}.
}

\author{
  Luke Tierney and R Core.

  Derived from the \CRANpkg{snow} package.
}
