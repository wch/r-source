% File src/library/parallel/man/makePSOCKcluster.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2003-2020 R Core Team
% Distributed under GPL 2 or later

\name{makeCluster}
\alias{makeCluster}
\alias{makePSOCKcluster}
\alias{makeForkCluster}
\alias{stopCluster}
\alias{setDefaultCluster}
\alias{getDefaultCluster}
\alias{R_PARALLEL_PORT}

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
getDefaultCluster()
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

  \code{makePSOCKcluster} is an enhanced version of
  \code{makeSOCKcluster} in package \CRANpkg{snow}.  It runs
  \command{Rscript} on the specified host(s) to set up a worker process
  which listens on a socket for expressions to evaluate, and returns the
  results (as serialized objects).

  \code{makeForkCluster} is merely a stub on Windows.  On Unix-alike
  platforms it creates the worker process by forking.

  The workers are most often running on the same host as the master,
  when no options need be set.

  Several options are supported (mainly for \code{makePSOCKcluster}):
  \describe{
    \item{\code{master}}{The host name of the master, as known to the
      workers.  This may not be the same as it is known to the master,
      and on private subnets it may be necessary to specify this as a
      numeric IP address.  For example, macOS is likely to detect a
      machine as \samp{somename.local}, a name known only to itself.}
    \item{\code{port}}{The port number for the socket connection,
      default taken from the environment variable \env{R_PARALLEL_PORT},
      then a randomly chosen port in the range \code{11000:11999}.}
    \item{\code{timeout}}{The timeout in seconds for that port.  This is
      the maximum time of zero communication between master and worker
      before failing.  Default is 30 days (and the POSIX standard only
      requires values up to 31 days to be supported).}
    \item{\code{setup_timeout}}{The maximum number of seconds a worker
      attempts to connect to master before failing.  Default is 2
      minutes.  The waiting time before the next attempt starts at
      0.1 seconds and is incremented 50\% after each retry.}
    \item{\code{outfile}}{Where to direct the \code{\link{stdout}} and
      \code{\link{stderr}} connection output from the workers.
      \code{""} indicates no redirection (which may only be useful for
      workers on the local machine).
      Defaults to \file{/dev/null} (\file{nul:} on Windows).  The other
      possibility is a file path on the worker's host.
      Files will be opened in append mode, as all workers log to the
      same file.}
    \item{\code{homogeneous}}{Logical, default true.  See \sQuote{Note}.}
    \item{\code{rscript}}{See \sQuote{Note}.}
    \item{\code{rscript_args}}{Character vector of additional
      arguments for \command{Rscript} such as \option{--no-environ}.}
    \item{\code{renice}}{A numerical \sQuote{niceness} to set for the
      worker processes, e.g.\sspace{}\code{15} for a low priority.
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
    \item{\code{setup_strategy}}{Character.  If \code{"parallel"} (default)
      workers will be started in parallel during cluster setup when this is
      possible, which is now for homogeneous \code{"PSOCK"} clusters with
      all workers started automatically (\code{manual = FALSE}) on the local
      machine.  Workers will be started sequentially on other clusters, on
      all clusters with \code{setup_strategy = "sequential"} and on \R 3.6.0
      and older.  This option is for expert use only (e.g.  debugging) and
      may be removed in future versions of R.}
  }

  Function \code{makeForkCluster} creates a socket cluster by forking
  (and hence is not available on Windows).  It supports options
  \code{port}, \code{timeout} and \code{outfile}, and always uses
  \code{useXDR = FALSE}. It is \emph{strongly discouraged} to use the
  \code{"FORK"} cluster with GUI front-ends  or multi-threaded libraries.
#ifdef unix
  See \code{\link{mcfork}} for details.
#endif

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
  For the cluster creators, an object of class
  \code{c("SOCKcluster", "cluster")}.

  For the default cluster setter and getter, the registered default
  cluster or \code{NULL} if there is no such cluster.
}

\note{
  Option \code{homogeneous = TRUE} was for years documented as
  \sQuote{Are all the hosts running identical setups?}, but this was
  apparently more restrictive than its author intended and not required
  by the code.
  
  The current interpretation of \code{homogeneous = TRUE} is that
  \command{Rscript} can be launched using the same path on each worker.
  That path is given by the option \code{rscript} and defaults to the
  full path to \command{Rscript} on the master.  (The workers are not
  required to be running the same version of \R as the master, nor even
  as each other.)

  For \code{homogeneous = FALSE}, \command{Rscript} on the workers is
  found on their default shell's path.
  
  For the very common usage of running both master and worker on a
  single multi-core host, the default settings are the appropriate ones.
}

\author{
  Luke Tierney and R Core.

  Derived from the \CRANpkg{snow} package.
}
