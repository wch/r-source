\name{tclServiceMode}
\alias{tclServiceMode}
\title{ Allow Tcl events to be serviced or not }
\description{
This function controls or reports on the Tcl service mode,
i.e. whether Tcl will respond to events.
}
\usage{
tclServiceMode(on = NULL)
}
\arguments{
  \item{on}{(logical) Whether event servicing is turned on. }
}
\details{
If called with \code{on == NULL} (the default), no change is made.
}
\value{
The value of the Tcl service mode before the call.
}
\examples{
\dontrun{    
    oldmode <- tclServiceMode(FALSE)
    # Do some work to create a nice picture.  Nothing will be displayed until...
    tclServiceMode(oldmode)
}
}
\keyword{ misc }
