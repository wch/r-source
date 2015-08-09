% File src/library/tools/man/find_gs_cmd.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2008-2014 R Core Team
% Distributed under GPL 2 or later

\name{find_gs_cmd}
\alias{find_gs_cmd}

\alias{R_GSCMD}
\alias{GSC}

\title{
  Find a GhostScript Executable
}
\description{
  Find a GhostScript executable in a cross-platform way.
}
\usage{
find_gs_cmd(gs_cmd = "")
}
\arguments{
  \item{gs_cmd}{The name, full or partial path of a GhostScript executable.}
}
\details{
  The details differ by platform.

  On a Unix-alike, the GhostScript executable is usually called
  \command{gs}.  The name (and possibly path) of the command is taken
  first from argument \code{gs_cmd} then from the environment variable
  \env{R_GSCMD} and default \code{gs}.  This is then looked for on the
  system path and the value returned if a match is found.

  On Windows, the name of the command is taken from argument
  \code{gs_cmd} then from the environment variables \env{R_GSCMD} and
  \env{GSC}.  If neither of those produces a suitable command name,
  \command{gswin64c} and \command{gswin32c} are tried in turn.  In all
  cases the command is looked for on the system \env{PATH}.

  Note that on Windows (and some other OSes) there are separate
  GhostScript executables to display Postscript/PDF files and to
  manipulate them: this function looks for the latter.  
}
\value{
  A character string giving the full path to a GhostScript executable if
  one was found, otherwise an empty string.
}
\examples{\dontrun{
## Suppose a Solaris system has GhostScript 9.00 on the path and
## 9.07 in /opt/csw/bin.  Then one might set
Sys.setenv(R_GSCMD = "/opt/csw/bin/gs")
}}
