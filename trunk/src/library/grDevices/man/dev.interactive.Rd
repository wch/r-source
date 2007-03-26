\name{dev.interactive}
\alias{dev.interactive}
\title{Is the Current Graphics Device Interactive ?}
\description{
  Test if the current graphics device is interactive.
}
\usage{
dev.interactive(orNone = FALSE)
}
\arguments{
  \item{orNone}{logical; if \code{TRUE}, the function also returns
    \code{TRUE} when \code{\link{.Device} == "null device"} and
    \code{\link{getOption}("device")} is among the interactive devices.}
}
\value{
  \code{dev.interactive()} returns a logical, \code{TRUE} if and only if an
  interactive (screen) device is in use.
}
\details{
  The \code{X11} (Unix), \code{windows} (Windows) and \code{quartz}
  (MacOS X) are regarded as interactive, together with \code{GTK} and
  \code{gnome} (used with the GNOME GUI: \code{GTK} is available in
  package \pkg{gtkDevice} and \code{gnome} is expected to be in a
  package \pkg{gnomeDevice}) and \code{JavaGD} (from the JGR project,
  see \url{http://stats.math.uni-augsburg.de/JGR/}).
}
\seealso{
  \code{\link{Devices}} for the available devices on your platform.
}
\examples{
dev.interactive # definition -> see list of "interactive" devices explicitly
}
\keyword{device}
