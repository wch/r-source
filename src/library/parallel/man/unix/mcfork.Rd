% File src/library/parallel/man/unix/mcfork.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2009-2018 R Core Team
% Distributed under GPL 2 or later

\name{mcfork}
\alias{mcfork}
\alias{mcexit}
\title{Fork a Copy of the Current R Process}
\description{
  These are low-level functions, not available on Windows, and not
  exported from the namespace.

  \code{mcfork} creates a new child process as a copy of the current \R process.

  \code{mcexit} closes the current child process, informing the master
  process as necessary.
}
\usage{
mcfork(estranged = FALSE)

mcexit(exit.code = 0L, send = NULL)
}
\arguments{
  \item{estranged}{logical, if \code{TRUE} then the new process has
    no ties to the parent process, will not show in the list of
    children and will not be killed on exit.}
  \item{exit.code}{process exit code.  By convention \code{0L} signifies
    a clean exit, \code{1L} an error.}
  \item{send}{if not \code{NULL} send this data before exiting
    (equivalent to using \code{\link{sendMaster}}).}
}
\details{
  The \code{mcfork} function provides an interface to the \code{fork}
  system call.  In addition it sets up a pipe between the master and
  child process that can be used to send data from the child process
  to the master (see \code{\link{sendMaster}}) and child's \file{stdin} is
  re-mapped to another pipe held by the master process (see
  \code{\link{sendChildStdin}}).

  If you are not familiar with the \code{fork} system call, do not use
  this function directly as it leads to very complex inter-process
  interactions amongst the \R processes involved.

  In a nutshell \code{fork} spawns a copy (child) of the current
  process, that can work in parallel to the master (parent)
  process.  At the point of forking both processes share exactly the
  same state including the workspace, global options, loaded packages
  etc.  Forking is relatively cheap in modern operating systems and no
  real copy of the used memory is created, instead both processes
  share the same memory and only modified parts are copied. This makes
  \code{mcfork} an ideal tool for parallel processing since there is no
  need to setup the parallel working environment, data and code is
  shared automatically from the start.

  \code{mcexit} is to be run in the child process.  It sends \code{send}
  to the master (unless \code{NULL}) and then shuts down the child
  process.  The child can also be shut down by sending it the signal
  \code{SIGUSR1}, as is done by the unexported function
  \code{parallel:::rmChild}.
}

\value{
  \code{mcfork} returns an object of the class \code{"childProcess"} to
  the master and of class \code{"masterProcess"} to the child: both the
  classes inherit from class \code{"process"}.  If \code{estranged} is
  set to \code{TRUE} then the child process will be of the class
  \code{"estrangedProcess"} and cannot communicate with the master
  process nor will it show up on the list of children. These are lists
  with components \code{pid} (the process id of the \emph{other}
  process) and a vector \code{fd} of the two file descriptor numbers
  for ends in the current process of the inter-process pipes.

  \code{mcexit} never returns.
}

\section{GUI/embedded environments}{
  It is \emph{strongly discouraged} to use \code{mcfork} and the
  higher-level functions which rely on it (e.g., \code{mcparallel},
  \code{mclapply} and \code{pvec}) in GUI or embedded environments,
  because it leads to several processes sharing the same GUI which will
  likely cause chaos (and possibly crashes).  Child processes should
  never use on-screen graphics devices.  Some precautions have been
  taken to make this usable in \command{R.app} on macOS, but users of
  third-party front-ends should consult their documentation.

  This can also apply to other connections (e.g., to an X server) created
  before forking, and to files opened by e.g.\sspace{}graphics devices.

  Note that \pkg{tcltk} counts as a GUI for these purposes since
  \command{Tcl} runs an event loop.  That event loop is inhibited in a
  child process but there could still be problems with Tk graphical
  connections.

  It is \emph{strongly discouraged} to use \code{mcfork} and the
  higher-level functions in any multi-threaded R process (with additional
  threads created by a third-party library or package).  Such use can lead
  to deadlocks or crashes, because the child process created by
  \code{mcfork} may not be able to access resources locked in the parent or
  may see an inconsistent version of global data (\code{mcfork} runs system
  call \code{fork} without \code{exec}).

  If in doubt, it is safer to use a non-FORK cluster (see
  \code{\link{makeCluster}}, \code{\link{clusterApply}}).
}

\author{
  Simon Urbanek and R Core.

  Derived from the \pkg{multicore} package formerly on \acronym{CRAN}.
}
\section{Warning}{
  This is a very low-level API for expert use only.
}
\seealso{
  \code{\link{mcparallel}}, \code{\link{sendMaster}}
}
% The results here involve a random PID.
\examples{\donttest{
## This will work when run as an example, but not when pasted in.
p <- parallel:::mcfork()
if (inherits(p, "masterProcess")) {
    cat("I'm a child! ", Sys.getpid(), "\n")
    parallel:::mcexit(,"I was a child")
}
cat("I'm the master\n")
unserialize(parallel:::readChildren(1.5))
}}
\keyword{interface}
