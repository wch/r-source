\name{getWindowsHandle}
\alias{getWindowsHandle}
\title{Get a Windows Handle}
\description{
  Get the Windows handle of a window or the R process.
}
\usage{
getWindowsHandle(which = "Console")
}
\arguments{
  \item{which}{A string (see below), or the number of a 
  		graphics device window}
}
\details{
  \code{getWindowsHandle} gets the Windows handle or process ID.  Possible choices for 
  \code{which} are:
  \tabular{ll}{
  \code{"Console"} \tab The console window handle. \cr
  \code{"Frame"}   \tab The MDI frame window handle. \cr
  \code{"Process"} \tab The process pseudo-handle. \cr
  \code{"ProcessId"} \tab The process identifier. \cr
  A device number \tab The window handle of a graphics device 
 }
  These values are not normally useful to users, but may be used by 
  developers making add-ons to \R{}.
  
  A zero is returned for the Frame handle if not running in MDI mode, 
  for the Console handle when running Rterm, for any unrecognized 
  string for \code{which}, or for a graphics device with no 
  corresponding window.
  
  Currently other windows (help browsers, etc.) are not accessible 
  through this function, but there are plans to make them available 
  later.
}
\value{
A one element integer vector holding the Windows handle. 
}
\seealso{\code{\link{getIdentification}}}
\examples{
getWindowsHandle()
}
\keyword{utilities}
