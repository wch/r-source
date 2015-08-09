% File src/library/grid/man/grid.locator.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{grid.locator}
\alias{grid.locator}
\title{ Capture a Mouse Click }
\description{
  Allows the user to click the mouse once within the current
  graphics
  device and returns the location of the mouse click within the
  current viewport, in the specified coordinate system.
}
\usage{
grid.locator(unit = "native")
}
\arguments{
  \item{unit}{ The coordinate system in which to return the location
    of the mouse click.
        See the \code{\link{unit}} function for valid coordinate systems.}
}
\details{
  This function is modal (like the graphics package
  function \code{locator}) so
  the command line and graphics drawing is blocked until
  the use has clicked the mouse  in the current device.
}
\value{
  A unit object representing the location of the mouse click within the
  current viewport, in the specified coordinate system.

  If the user did not click mouse button 1, the function
  (invisibly) returns \code{NULL}.
}
\author{ Paul Murrell }
\seealso{ \code{\link{viewport}}, \code{\link{unit}},
  \code{locator} in package \pkg{graphics}, and for an application
  see \code{\link[lattice:interaction]{trellis.focus}} and
  \code{\link[lattice:interaction]{panel.identify}} in package
  \CRANpkg{lattice}.
}
\examples{
if (interactive()) {
  ## Need to write a more sophisticated unit as.character method
  unittrim <- function(unit) {
    sub("^([0-9]+|[0-9]+[.][0-9])[0-9]*", "\\\\1", as.character(unit))
  }
  do.click <- function(unit) {
    click.locn <- grid.locator(unit)
    grid.segments(unit.c(click.locn$x, unit(0, "npc")),
                  unit.c(unit(0, "npc"), click.locn$y),
                  click.locn$x, click.locn$y,
                  gp=gpar(lty="dashed", col="grey"))
    grid.points(click.locn$x, click.locn$y, pch=16, size=unit(1, "mm"))
    clickx <- unittrim(click.locn$x)
    clicky <- unittrim(click.locn$y)
    grid.text(paste0("(", clickx, ", ", clicky, ")"),
              click.locn$x + unit(2, "mm"), click.locn$y,
              just="left")
  }
  do.click("inches")
  pushViewport(viewport(width=0.5, height=0.5,
                         xscale=c(0, 100), yscale=c(0, 10)))
  grid.rect()
  grid.xaxis()
  grid.yaxis()
  do.click("native")
  popViewport()
}
}
\keyword{dplot}
