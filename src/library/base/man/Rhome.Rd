% File src/library/base/man/Rhome.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{Rhome}
\alias{R.home}
\alias{R_HOME}
\title{Return the R Home Directory}
\description{
  Return the \R home directory, or the full path to a component of the
  \R installation.
}
\usage{
R.home(component = "home")
}
\arguments{
  \item{component}{As well as \code{"home"} which gives the \R home
    directory, other known values are \code{"bin"}, \code{"doc"},
    \code{"etc"}, \code{"include"}, \code{"modules"} and \code{"share"}
    giving the paths to the corresponding parts of an \R installation.}
}
\details{
  The \R home directory is the top-level directory of the \R
  installation being run.

  The \R home directory is often referred to as \var{R_HOME},
  and is the value of an environment variable of that name in an \R
  session.
  It can be found outside an \R session by \code{R \link{RHOME}}.
}
\value{
  A character string giving the \R home directory or path to a
  particular component.  Normally the components are all subdirectories
  of the \R home directory, but this need not be the case in a Unix-like
  installation.

  The value for \code{"modules"} and on Windows \code{"bin"} is a
  sub-architecture-specific location.

  On a Unix-alike, the constructed paths are based on the current
  values of the environment variables \env{R_HOME} and where set
  \env{R_SHARE_DIR}, \env{R_DOC_DIR} and \env{R_INCLUDE_DIR} (these are
  set on startup and should not be altered).

  On Windows the values of \code{R.home()} and \env{R_HOME} are
  switched to the 8.3 short form of path elements if required and if
  the Windows service to do that is enabled.  The value of
  \env{R_HOME} is set to use forward slashes (since many package
  maintainers pass it unquoted to shells, for example in
  \file{Makefile}s).  
}
\keyword{utilities}
