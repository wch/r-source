\name{dev.interactive}
\alias{dev.interactive}
\title{Test if an Interactive Graphics Device is in Use}
\description{
  Test if an interactive graphics device is in use.
}
\usage{
dev.interactive()
}
\value{
  \code{dev.interactive()} returns a logical, \code{TRUE} iff an
  interactive (screen) device is in use.
}
\details{
  The \code{X11} (Unix), \code{windows} (Windows) and \code{quartz}
  (MacOS X) are regarded as interactive, together with \code{GTK} and
  \code{gnome} (used with the GNOME GUI: \code{GTK} is available in
  package \pkg{gtkDevice} and \code{gnome} is expected to be in a
  package \pkg{gnomeDevice}).
}
\seealso{
  \code{\link{Devices}} for the available devices on your platform.
}
\keyword{device}
