% File src/library/base/man/Sys.which.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2007-12 R Core Team
% Distributed under GPL 2 or later

\name{Sys.which}
\alias{Sys.which}
\title{Find Full Paths to Executables}
\description{
  This is an interface to the system command \code{which}, or to an
  emulation on Windows.
}
\usage{
Sys.which(names)
}
\arguments{
  \item{names}{Character vector of names or paths of possible executables.}
}
\details{
  The system command \command{which} reports on the full path names of
  an executable (including an executable script) as would be executed by
  a shell, accepting either absolute paths or looking on the path.

  On Windows an \sQuote{executable} is a file with extension
  \file{.exe}, \file{.com}, \file{.cmd} or \file{.bat}.  Such files need
  not actually be executable, but they are what \code{\link{system}}
  tries.

  On a Unix-alike the full path to \code{which} (usually
  \file{/usr/bin/which}) is found when \R is installed.
}
\value{
  A character vector of the same length as \code{names}, named by
  \code{names}.  The elements are either the full path to the
  executable  or some indication that no executable of that name was
  found.  Typically the indication is \code{""}, but this does depend on
  the OS (and the known exceptions are changed to \code{""}).  Missing
  values in \code{names} have missing return values (as from \R 3.0.0).

  On Windows the paths will be short paths (8+3 components, no spaces)
  with \code{\\} as the path delimiter.
}
\note{
  Except on Windows this calls the system command \command{which}: since
  that is not part of e.g. the POSIX standards, exactly what it does is
  OS-dependent.  It will usually do tilde-expansion and it may make use
  of \command{csh} aliases.

  In \R 2.x.y, arguments containing spaces or other metacharacters
  needed to be escaped as they would be for a shell: for example Windows
  paths containing spaces needed to be enclosed in double quotes.
}
\examples{
## the first two are likely to exist everywhere
## texi2dvi exists on most Unix-alikes and under MiKTeX
Sys.which(c("ftp", "ping", "texi2dvi", "this-does-not-exist"))
}
\keyword{ utilities }
