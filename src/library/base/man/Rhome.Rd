% File src/library/base/man/Rhome.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{Rhome}
\alias{R.home}
\alias{R_HOME}
\title{Return the R Home Directory}
\description{
  Return the \R home directory.
}
\usage{
R.home(component = "home")
}
\arguments{
  \item{component}{As well as \code{"home"} which gives the \R home
    directory, other known values are \code{"bin"}, \code{"doc"},
    \code{"etc"}, \code{"modules"} and \code{"share"} giving the paths to the
    corresponding parts of an \R installation.}
}
\details{
  The \R home directory is the top-level directory of the \R
  installation being run.

  The \R home directory is often referred to as \var{R_HOME},
  and is the value of an environment variable of that name in an \R
  session.
#ifdef unix
  It can be found outside an \R session by \code{R \link{RHOME}}.
#endif
}
\value{
  A character string giving the \R home directory or path to a
  particular component.  Normally the components are all subdirectories
  of the \R home directory, but this may not be the case in a Unix-like
  installation.

  The return value for \code{"modules"} and on Windows \code{"bin"} is
  to a sub-architecture-specific location.

  The function \code{R.home()} bases the constructed paths on the
  current value of the environment variable \env{R_HOME} which is
  normally set on startup.

  On Windows the values of \code{R.home()} and \env{R_HOME} are
  guaranteed not to contain spaces, switching to the 8.3 short form of
  path elements if required.  The value of \env{R_HOME}
  is set on startup to use forward slashes (since many package
  maintainers pass it unquoted to shells, for example in \file{Makefile}s).
}
\keyword{utilities}
