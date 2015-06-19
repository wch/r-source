% File src/library/grDevices/man/dev.interactive.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{dev.interactive}
\alias{dev.interactive}
\alias{deviceIsInteractive}
\title{Is the Current Graphics Device Interactive?}
\description{
  Test if the current graphics device (or that which would be opened) is
  interactive.
}
\usage{
dev.interactive(orNone = FALSE)

deviceIsInteractive(name = NULL)
}
\arguments{
  \item{orNone}{logical; if \code{TRUE}, the function also returns
    \code{TRUE} when \code{\link{.Device} == "null device"} and
    \code{\link{getOption}("device")} is among the known
    interactive devices.}
  \item{name}{one or more device names as a character vector,
    or \code{NULL} to give the existing list.}
}
\details{
  The \code{X11} (Unix), \code{windows} (Windows) and \code{quartz}
  (OS X, on-screen types only) are regarded as interactive, together
  with \code{JavaGD} (from the package of the same name) and
  \code{CairoWin} and \code{CairoX11} (from package \CRANpkg{Cairo}).
  Packages can add their devices to the list by calling
  \code{deviceIsInteractive}.
}
\value{
  \code{dev.interactive()} returns a logical, \code{TRUE} if and only if an
  interactive (screen) device is in use.

  \code{deviceIsInteractive} returns the updated list of known
  interactive devices, invisibly unless \code{name = NULL}.
}
\seealso{
  \code{\link{Devices}} for the available devices on your platform.
}
\examples{
dev.interactive()
print(deviceIsInteractive(NULL))
}
\keyword{device}
