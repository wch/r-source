% File src/library/grid/man/grid.clip.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{grid.clip}
\alias{grid.clip}
\alias{clipGrob}
\title{Set the Clipping Region}
\description{
  These functions set the clipping region within the current viewport
  \emph{without} altering the current coordinate system.
}
\usage{
grid.clip(...)
clipGrob(x = unit(0.5, "npc"), y = unit(0.5, "npc"),
         width = unit(1, "npc"), height = unit(1, "npc"),
         just = "centre", hjust = NULL, vjust = NULL,
         default.units = "npc", name = NULL, vp = NULL)
}
\arguments{
  \item{x}{A numeric vector or unit object specifying x-location.}
  \item{y}{A numeric vector or unit object specifying y-location.}
  \item{width}{A numeric vector or unit object specifying width.}
  \item{height}{A numeric vector or unit object specifying height.}
  \item{just}{The justification of the clip rectangle
    relative to its (x, y) location.  If there are two values, the first
    value specifies horizontal justification and the second value specifies
    vertical justification.  Possible string values are: \code{"left"},
    \code{"right"}, \code{"centre"}, \code{"center"}, \code{"bottom"},
    and \code{"top"}.  For numeric values, 0 means left alignment
    and 1 means right alignment.
  }
  \item{hjust}{A numeric vector specifying horizontal justification.
    If specified, overrides the \code{just} setting.}
  \item{vjust}{A numeric vector specifying vertical justification.
    If specified, overrides the \code{just} setting.}
  \item{default.units}{A string indicating the default units to use
    if \code{x}, \code{y}, \code{width}, or \code{height}
    are only given as numeric vectors.}
  \item{name}{ A character identifier. }
  \item{vp}{A Grid viewport object (or NULL).}
  \item{\dots}{Arguments passed to \code{clipGrob}.}
}
\details{
  Both functions create a clip rectangle (a graphical object describing
  a clip rectangle), but only \code{grid.clip}
  enforces the clipping.

  Pushing or popping a viewport \emph{always} overrides the clip
  region set by a clip grob, regardless of whether that viewport
  explicitly enforces a clipping region.
}
\value{
  \code{clipGrob} returns a clip grob.
}
\author{Paul Murrell}
\seealso{
  \link{Grid},
  \code{\link{viewport}}
}
\examples{
# draw across entire viewport, but clipped
grid.clip(x = 0.3, width = 0.1)
grid.lines(gp=gpar(col="green", lwd=5))
# draw across entire viewport, but clipped (in different place)
grid.clip(x = 0.7, width = 0.1)
grid.lines(gp=gpar(col="red", lwd=5))
# Viewport sets new clip region
pushViewport(viewport(width=0.5, height=0.5, clip=TRUE))
grid.lines(gp=gpar(col="grey", lwd=3))
# Return to original viewport;  get
# clip region from previous grid.clip()
# (NOT from previous viewport clip region)
popViewport()
grid.lines(gp=gpar(col="black"))
}
\keyword{dplot}
