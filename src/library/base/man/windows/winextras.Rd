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
\details{
  \code{win.version} is an auxiliary function for
  \code{\link{bug.report}} which returns a character string describing
  the version of Windows in use.

  \code{zip.unpack} unpacks the zip file \code{zipname} in directory
  \code{dest}: it is an internal version of \code{unzip zipfile -d dest}
  (but will use an external \code{unzip} if one is set in
  \code{options("unzip")}.
}
\keyword{utilities}
