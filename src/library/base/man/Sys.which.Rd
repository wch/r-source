% File src/library/base/man/Sys.which.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2007-10 R Core Development Team
% Distributed under GPL 2 or later

\name{Sys.which}
\alias{Sys.which}
\title{Find Full Paths to Executables}
\description{
  This is an interface to the system command \code{which}.
}
\usage{
Sys.which(names)
}
\arguments{
  \item{names}{Character vector of names of possible executables.}
}
\details{
  The system command \code{which} reports on the full names of an
  executable (including an executable script) found on the current path.

  On Windows an \sQuote{executable} is a file with extension
  \file{.exe}, \file{.com}, \file{.cmd} or \file{.bat}.  Such files need
  not actually be executable, but they are what \code{\link{system}}
  tries.
  
  On a Unix-alike the full path to \code{which} (usually
  \file{/usr/bin/which}) is found when \R is installed and (currently)
  stored in environment variable \code{WHICH}.
}
\value{
  A character vector of the same length as \code{names}, named by
  \code{names}.  The elements are either the full path to the
  executable  or some indication that no executable of that name was
  found.  Typically the indication is \code{""}, but this does depend on
  the OS (and the known exceptions are changed to \code{""} as from \R 2.12.0).

#ifdef windows
  The paths will be short paths (8+3 components, no spaces) with
  \code{\\} as the path delimiter.
#endif  
}
\examples{
## the first two are likely to exist everywhere
## texi2dvi exists on most Unix-alikes and under MiKTeX
Sys.which(c("ftp", "ping", "texi2dvi", "this-does-not-exist"))
}
\keyword{ utilities }
