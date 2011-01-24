% File src/library/utils/man/windows/winextras.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Development Team
% Distributed under GPL 2 or later

\name{winextras}
\alias{win.version}
\alias{zip.unpack}
\title{Auxiliary Functions for the Windows Port}
\description{
  Auxiliary functions for the Windows port
}
\usage{
win.version()
zip.unpack(zipname, dest)
}
\arguments{
  \item{zipname}{character string giving name of zip file.}
  \item{dest}{character string giving directory within which to unpack.}
}
\details{
  \code{win.version} is an auxiliary function for
  \code{\link{bug.report}} which returns a character string describing
  the version of Windows in use.

  As from \R 2.13.0 \code{zip.unpack} is deprecated: used
  \code{\link{unzip}} (available since \R 2.9.0) instead.

  \code{zip.unpack} unpacks the zip file \code{zipname} in directory
  \code{dest}: it is an internal version of \code{unzip zipfile -d dest}
  (but will use an external \code{unzip} if one is set in
  \code{options("unzip")}).  The result is an error code, with an
  attribute \code{"extracted"} listing the full paths to the extracted files.
}
\keyword{utilities}
