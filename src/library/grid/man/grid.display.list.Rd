\name{grid.display.list}
\alias{grid.display.list}
\title{Control the Grid Display List}
\description{
  Turn the Grid display list on or off.
}
\usage{
grid.display.list(on=TRUE)
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
}
\value{
  None.
}
\author{Paul Murrell}

\section{WARNING}{
  Turning the display list on causes the display list to be erased!
}
\keyword{dplot}
