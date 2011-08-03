% File src/library/parallel/man/mcfork.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2009-11 R Core Development Team
% Distributed under GPL 2 or later

\name{mcfork}
\alias{mcfork}
\alias{mcexit}
\title{Fork a Copy of the Current R Process}
\description{
  These are low-level functions, not available on Windows.
  
  \code{mcfork} creates a new child process as a copy of the current \R process.
 
  \code{mcexit} closes the current child process, informing the master
  process as necessary.
}
\usage{
mcfork()

mcexit(exit.code = 0L, send = NULL)
}
\arguments{
  \item{exit.code}{process exit code.  By convention \code{0L} signifies
    clean exit, \code{1L} an error.}
  \item{send}{if not \code{NULL} send this data before exiting
    (equivalent to using \code{\link{sendMaster}})}
}
\details{
  The \code{mcfork} function provides an interface to the \code{fork}
  system call.  In addition it sets up a pipe between the master and
  child process that can be used to send data from the child process
  to the master (see \code{\link{sendMaster}}) and child's stdin is
  re-mapped to another pipe held by the master process (see
  \code{\link{sendChildStdin}}).

  If you are not familiar with the \code{fork} system call, do not
  use this function since it leads to very complex inter-process
  interactions amongst the R processes involved.

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

  It is \emph{strongly discouraged} to use \code{mcfork} in GUI or
  embedded environments, because it leads to several processes sharing
  the same GUI which will likely cause chaos (and possibly
  crashes).  Child processes should never use on-screen graphics
  devices.
}
\value{
  \code{mcfork} returns an object of the class \code{"childProcess"} to
  the master and of class \code{"masterProcess"} to the child.

  \code{mcexit} never returns.
}
\section{Warning}{
  This is a very low-level API for expert use only. 
}
\seealso{
  \code{\link{parallel}}, \code{\link{sendMaster}}
}
% The results here involve a random PID.
\examples{\donttest{
## This will work when run as an example, but not when pasted in.
p <- mcfork()
if (inherits(p, "masterProcess")) {
    cat("I'm a child! ", Sys.getpid(), "\n")
    mcexit(,"I was a child")
}
cat("I'm the master\n")
unserialize(readChildren(1.5))
}}
\author{Simon Urbanek}
\keyword{interface}
