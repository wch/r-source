% File src/library/parallel/man/unix/children.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2009-2011 R Core Team
% Distributed under GPL 2 or later

\name{mcchildren}
\alias{children}
\alias{readChild}
\alias{readChildren}
\alias{selectChildren}
\alias{sendChildStdin}
\alias{sendMaster}
\alias{mckill}
\title{Low-level Functions for Management of Forked Processes}
\description{
  These are low-level support functions for the forking approach.

  They are not available on Windows, and not exported from the namespace.
}
\usage{
children(select)
readChild(child)
readChildren(timeout = 0)
selectChildren(children = NULL, timeout = 0)
sendChildStdin(child, what)
sendMaster(what)

mckill(process, signal = 2L)
}
\arguments{
  \item{select}{if omitted, all active children are returned, otherwise
    \code{select} should be a list of processes and only those from the
    list that are active will be returned.}
  \item{child}{child process (object of the class \code{"childProcess"}) or a
    process ID (pid).  See also \sQuote{Details}.}
  \item{timeout}{timeout (in seconds, fractions supported) to wait
    for a response before giving up.}
  \item{children}{list of child processes or a single child process
    object or a vector of process IDs or \code{NULL}.  If \code{NULL}
    behaves as if all currently known children were supplied.}
  \item{what}{For \code{sendChildStdin}:\cr
    Character or raw vector.  In the former case elements are
    collapsed using the newline character.  (But no trailing newline is
    added at the end!)

    For \code{sendMaster}:\cr
    Data to send to the master process.  If \code{what} is not
    a raw vector, it will be serialized into a raw vector.  Do NOT
    send an empty raw vector -- that is reserved for internal use.}
  \item{process}{process (object of the class \code{process}) or a
    process ID (pid)}
  \item{signal}{integer: signal to send.  Values of 2 (SIGINT), 9
    (SIGKILL) and 15 (SIGTERM) are pretty much portable, but for maximal
    portability use \code{tools::\link{SIGTERM}} and so on.}
}
\details{
  \code{children} returns currently active children.

  \code{readChild} reads data from a given child process.

  \code{selectChildren} checks children for available data.

  \code{readChildren} checks all children for available data and reads
  from the first child that has available data.

  \code{sendChildStdin} sends a string (or data) to one or more child's
  standard input.  Note that if the master session was interactive, it
  will also be echoed on the standard output of the master process
  (unless disabled).  The function is vector-compatible, so you can
  specify \code{child} as a list or a vector of process IDs.

  \code{sendMaster} sends data from the child to the master process.

  \code{mckill} sends a signal to a child process: it is equivalent to
  \code{\link{pskill}} in package \pkg{tools}.
}
\value{
  \code{children} returns a (possibly empty) list of objects of class
  \code{"process"}, the process ID.

  \code{readChild} and \code{readChildren} return a raw vector with a
  \code{"pid"} attribute if data were available, an integer vector of
  length one with the process ID if a child terminated or \code{NULL}
  if the child no longer exists (no children at all for
  \code{readChildren}).

  \code{selectChildren} returns \code{TRUE} is the timeout was reached,
  \code{FALSE} if an error occurred (e.g. if the master process was
  interrupted) or an integer vector of process IDs with children that
  have data available, or \code{NULL} if there are no children.

  \code{sendChildStdin} returns a vector of \code{TRUE} values (one for
  each member of \code{child}) or throws an error.

  \code{sendMaster} returns \code{TRUE} or throws an error.

  \code{mckill} returns \code{TRUE}.
}
\author{
  Simon Urbanek and R Core.

  Derived from the \CRANpkg{multicore} package.
}
\section{Warning}{
  This is a very low-level API for expert use only.
}
\seealso{
  \code{\link{mcfork}}, \code{\link{sendMaster}}, \code{\link{mcparallel}}
}
\examples{\dontrun{
p  <- mcparallel(scan(n = 1, quiet = TRUE))
sendChildStdin(p, "17.4\n")
mccollect(p)[[1]]
}}
\keyword{interface}
