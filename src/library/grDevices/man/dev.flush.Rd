% File src/library/grDevices/man/devSet.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2011-2013 R Core Team
% Distributed under GPL 2 or later

\name{dev.flush}
\alias{dev.hold}
\alias{dev.flush}
\title{
  Hold or Flush Output on an On-Screen Graphics Device.
}
\description{
  This gives a way to hold/flush output on certain on-screen devices,
  and is ignored by other devices.
}
\usage{
dev.hold(level = 1L)
dev.flush(level = 1L)
}
\arguments{
  \item{level}{Integer >= 0.  The amount by which to change the hold
    level.  Negative values will be silently replaced by zero.}
}
\details{
  Devices which implement this maintain a stack of hold levels: calling
  \code{dev.hold} increases the level and \code{dev.flush} decreases it.
  Calling \code{dev.hold} when the hold level is zero increases the hold
  level and inhibits graphics display.  When calling \code{dev.flush}
  clears all pending holds the screen display is refreshed and normal
  operation is resumed.

  This is implemented for the cairo-based \code{X11} types
  with buffering.  When the hold level is positive the \sQuote{watch}
  cursor is set on the device's window.

  It is available on the \code{quartz} device on OS X.

  This is implemented for the \code{windows} device with
  buffering selected (the default).  When the hold level is positive the
  \sQuote{busy} cursor is set on the device's window.
}

\value{
  The current level after the change, invisibly.  This is \code{0} on
  devices where hold levels are not supported.
}
\keyword{ dplot }
