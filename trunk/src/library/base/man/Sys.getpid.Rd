% File src/library/base/man/Sys.getpid.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{Sys.getpid}
\alias{Sys.getpid}

\title{Get the Process ID of the R Session}
\description{
  Get the process ID of the \R Session.  It is guaranteed by the
  operating system that two \R sessions running simultaneously will
  have different IDs, but it is possible that \R sessions running at
  different times will have the same ID.
}
\usage{
Sys.getpid()
}
\value{
  An integer, often between 1 and 32767 under Unix-alikes (but for
  example FreeBSD and macOS use IDs up to 99999) and a
  positive integer (up to 32767) under Windows.
}
\examples{\donttest{
Sys.getpid()

## Show files opened from this R process
if(.Platform$OS.type == "unix") ## on Unix-alikes such Linux, macOS, FreeBSD:
   system(paste("lsof -p", Sys.getpid()))
}}
\keyword{utilities}
