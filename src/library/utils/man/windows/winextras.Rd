% File src/library/utils/man/windows/winextras.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Development Team
% Distributed under GPL 2 or later

\name{winextras}
\alias{win.version}
\title{Auxiliary Functions for the Windows Port}
\description{
  Auxiliary functions for the Windows port
}
\usage{
win.version()
}
\arguments{
  \item{zipname}{character string giving name of zip file.}
  \item{dest}{character string giving directory within which to unpack.}
}
\details{
  \code{win.version} is an auxiliary function for
  \code{\link{bug.report}} which returns a character string describing
  the version of Windows in use.
}
\keyword{utilities}
