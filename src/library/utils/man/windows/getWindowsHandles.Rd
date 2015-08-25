% File src/library/utils/man/windows/getWindowsHandles.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2009-10 Duncan Murdoch and the R Core Team
% Distributed under GPL 2 or later

\name{getWindowsHandles}
\alias{getWindowsHandles}
\title{
  Get handles of windows.
}
\description{
  This function gets the Windows handles of visible top level windows or
  windows within the \R MDI frame.
}
\usage{
getWindowsHandles(which = "R", pattern = "", minimized = FALSE)
}
\arguments{
  \item{which}{
    A vector of strings "R" or "all" (possibly with repetitions).  See
    the Details section.
  }
  \item{pattern}{
    A vector of patterns that the titles of the windows must match.
  }
  \item{minimized}{
    A logical vector indicating whether minimized windows should be considered.
  }
}
\details{
  This function will search for Windows handles, for passing to external
  GUIs or to the \code{\link{arrangeWindows}} function.  Each of the
  arguments may be a vector of values.  These will be treated as
  follows:
  \itemize{
    \item The arguments will all be recycled to the same length.
    \item The corresponding elements of each argument will be applied in
    separate searches.
    \item The final result will be the union of the windows identified in
    each of the searches.
  }

  If an element of \code{which} is \code{"R"}, only windows belonging to
  the current \R process will be returned.  In MDI mode, those will be the
  child windows within the \R GUI frame.  In SDI mode, all windows
  belonging to the process will be included.

  If the element is \code{"all"}, then top level windows will be returned.

  The elements of \code{pattern} will be used to make a subset of windows
  whose title text matches (according to \code{\link{grep}}) the pattern.

  If \code{minimized = FALSE}, minimized windows will be ignored.
}
\value{
  A list of external pointers containing the window handles.
}
\note{
  This is only available on Windows.
}
\author{
  Duncan Murdoch
}
\seealso{
  \code{\link{arrangeWindows}}
}
\examples{\donttest{
getWindowsHandles()
getWindowsHandles("all")
}}
\keyword{ utilities }
