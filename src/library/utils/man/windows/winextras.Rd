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

  \code{zip.unpack} unpacks the zip file \code{zipname} in directory
  \code{dest}: it is an internal version of \code{unzip zipfile -d dest}
  (but will use an external \code{unzip} if one is set in
  \code{options("unzip")}).  The result is an error code, with an
  attribute \code{"extracted"} listing the full paths to the extracted files.
}
\keyword{utilities}
