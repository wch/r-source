% File src/library/utils/man/windows/arrangeWindows.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2009-2013 Duncan Murdoch and the R Core Team
% Distributed under GPL 2 or later

\name{arrangeWindows}
\alias{arrangeWindows}
\title{
Rearrange windows in the R GUI.
}
\description{
This function allows you to tile or cascade windows, or to minimize or restore them.
}
\usage{
arrangeWindows(action, windows, preserve = TRUE, outer = FALSE)
}
\arguments{
  \item{action}{
  The action to perform on the windows.  The choices are \code{c("vertical", "horizontal", "cascade", "minimize", 
    "restore")} with default \code{"vertical"}; see the Details below for the interpretation.
    Abbreviations may be used.
    }
  \item{windows}{
  A list of window handles, by default produced by \code{\link{getWindowsHandles}()}.  
}
  \item{preserve}{
  If \code{TRUE}, when tiling preserve the outer boundary of the collection of windows; otherwise 
  make them as large as will fit.
}
  \item{outer}{
  This argument is only used in MDI mode.  If \code{TRUE}, tile the windows on the system
  desktop.  Otherwise, tile them within the MDI frame.
}
}
\details{
The actions are as follows:
\describe{
\item{\code{"vertical"}}{Tile vertically.}
\item{\code{"horizontal"}}{Tile horizontally.}
\item{\code{"cascade"}}{Cascade the windows.}
\item{\code{"minimize"}}{Minimize all of the windows.}
\item{\code{"restore"}}{Restore all of the windows to normal size (not minimized, not maximized).}
}
The tiling and cascading are done by the standard Windows API functions, but unlike those functions,
they will apply to all of the windows in the \code{windows} list.  

By default, \code{windows} is set to the result of
\code{\link{getWindowsHandles}()} (with one exception described
below).  This will select windows belonging to the current \R process.
However, if the global environment contains a variable named
\code{.arrangeWindowsDefaults}, it will be used as the argument list
instead. See the \code{\link{getWindowsHandles}} man page for a
discussion of the optional arguments to that function.

When \code{action = "restore"} is used with \code{windows} unspecified,
\code{minimized = TRUE} is added to the argument list of
\code{\link{getWindowsHandles}} so that minimized windows will be restored.

In MDI mode, by default tiling and cascading will happen within the R
GUI frame. However, if \code{outer = TRUE}, tiling is done on the system
desktop. This will generally not give desirable results if any R child
windows are included within \code{windows}.

}

\note{
  This is only available on Windows.
}

\value{
This function is called for the side effect of arranging the windows.
The list of window handles is returned invisibly.
}
\author{
Duncan Murdoch
}

\seealso{
\code{\link{getWindowsHandles}}
}
\examples{
\dontrun{
arrangeWindows("v")
# This default is useful only in SDI mode:  it will tile any Firefox window 
# along with the R windows
.arrangeWindowsDefaults <- list(c("R", "all"), pattern = c("", "Firefox"))
arrangeWindows("v")
}
}
\keyword{utilities}
