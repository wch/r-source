% File src/library/tcltk/man/tclServiceMode.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{tclServiceMode}
\alias{tclServiceMode}
\title{ Allow Tcl events to be serviced or not }
\description{
This function controls or reports on the Tcl service mode,
i.e., whether Tcl will respond to events.
}
\usage{
tclServiceMode(on = NULL)
}
\arguments{
  \item{on}{(logical) Whether event servicing is turned on. }
}
\details{
  If called with \code{on == NULL} (the default), no change is made.

  Note that this blocks all Tcl/Tk activity, including for widgets from
  other packages.  It may be better to manage mapping of windows individually.
}
\value{
The value of the Tcl service mode before the call.
}
\examples{
## see demo(tkcanvas) for an example
\dontrun{
oldmode <- tclServiceMode(FALSE)
# Do some work to create a nice picture.
# Nothing will be displayed until...
tclServiceMode(oldmode)
}
## another idea is to use tkwm.withdraw() ... tkwm.deiconify()
}
\keyword{ misc }
