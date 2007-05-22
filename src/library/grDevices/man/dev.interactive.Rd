\name{dev.interactive}
\alias{dev.interactive}
\alias{deviceIsInteractive}
\title{Is the Current Graphics Device Interactive ?}
\description{
  Test if the current graphics device (or that which would be opened) is
  interactive.
}
\usage{
dev.interactive(orNone = FALSE)

deviceIsInteractive(name)
}
\arguments{
  \item{orNone}{logical; if \code{TRUE}, the function also returns
    \code{TRUE} when \code{\link{.Device} == "null device"} and
    \code{\link{getOption}("device")} is among the known
    interactive devices.}
  \item{name}{one of more device names, as a character vector, or \code{NULL}.}
}
\details{
  There is a list of known interactive devices, and in
  addition a current device is regarded as interactive if its
  displaylist is enabled (see \code{\link{dev.control}}).
  
  The \code{X11} (Unix), \code{windows} (Windows) and \code{quartz}
  (MacOS X) are regarded as interactive, together with \code{GTK}
  (available in package \pkg{gtkDevice}) and \code{JavaGD} (from the
  package of the same name).  Packages can add their devices to the list
  by calling \code{deviceIsInteractive}.
}
\value{
  \code{dev.interactive()} returns a logical, \code{TRUE} if and only if an
  interactive (screen) device is in use.

  \code{deviceIsInteractive} returns the updated list of known
  interactive devices, invisibly.
}
\seealso{
  \code{\link{Devices}} for the available devices on your platform.
}
\examples{
dev.interactive()
print(deviceIsInteractive(NULL))
}
\keyword{device}
