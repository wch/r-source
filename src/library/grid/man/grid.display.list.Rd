% File src/library/grid/man/grid.display.list.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{grid.display.list}
\alias{grid.display.list}
\alias{engine.display.list}
\title{Control the Grid Display List}
\description{
  Turn the Grid display list on or off.
}
\usage{
grid.display.list(on=TRUE)
engine.display.list(on=TRUE)
}
\arguments{
  \item{on}{A logical value to indicate whether the display list
    should be on or off.}
}
\details{
  All drawing and viewport-setting operations are (by default)
  recorded in the Grid display list.  This allows redrawing
  to occur following an editing operation.

  This display list could get very large so it may be useful to
  turn it off in some cases;  this will of course disable redrawing.

  All graphics output is also recorded on the main display list
  of the R graphics engine (by default).  This supports redrawing
  following a device resize and allows copying between devices.

  Turning off this display list means that grid will redraw from its
  own display list for device resizes and copies.  This will be slower
  than using the graphics engine display list.
}
\value{
  None.
}
\author{Paul Murrell}

\section{WARNING}{
  Turning the display list on causes the display list to be erased!

  Turning off both the grid display list and the graphics engine
  display list will result in no redrawing whatsoever.
}
\keyword{dplot}
