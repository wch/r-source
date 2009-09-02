% File src/library/base/man/Sys.getpid.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
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
  An integer, usually an integer between 1 and 32767 under
  Unix-alikes and a small positive integer under Windows.
}
\examples{
Sys.getpid()
}
\keyword{utilities}
