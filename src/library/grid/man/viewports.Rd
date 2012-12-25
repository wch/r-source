% File src/library/grid/man/viewports.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{Working with Viewports}
\alias{pushViewport}
\alias{downViewport}
\alias{seekViewport}
\alias{popViewport}
\alias{upViewport}
\title{Maintaining and Navigating the Grid Viewport Tree}
\description{
  Grid maintains a tree of viewports --- nested drawing
  contexts.

  These functions provide ways to add or remove viewports
  and to navigate amongst viewports in the tree.
}
\usage{
pushViewport(..., recording=TRUE)
popViewport(n, recording=TRUE)
downViewport(name, strict=FALSE, recording=TRUE)
seekViewport(name, recording=TRUE)
upViewport(n, recording=TRUE)
}
\arguments{
  \item{\dots}{One or more objects of class \code{"viewport"}.}
  \item{n}{An integer value indicating how many viewports to
    pop or navigate up.  The special value \code{0} indicates to
    pop or navigate viewports right up to the root viewport.}
  \item{name}{A character value to identify a viewport in the tree.}
  \item{strict}{ A boolean indicating whether the vpPath must be
    matched exactly. }
  \item{recording}{A logical value to indicate whether the viewport
    operation should be recorded on the Grid display list.}
}
\details{
  Objects created by the \code{viewport()} function are only
  descriptions
  of a drawing context.  A viewport object must be pushed onto the
  viewport tree before it has any effect on drawing.

  The viewport tree always has a single root viewport (created by the
  system) which corresponds to the entire device (and default
  graphical parameter settings).  Viewports may be added to the tree
  using \code{pushViewport()} and removed from the tree using
  \code{popViewport()}.

  There is only ever one current viewport, which is the current
  position within the viewport tree.  All drawing and
  viewport operations are relative to the current viewport.
  When a viewport is pushed it becomes the current viewport.
  When a viewport is popped, the parent viewport becomes
  the current viewport.  Use \code{upViewport} to navigate to the parent
  of the current viewport, without removing the current viewport
  from the viewport tree.  Use \code{downViewport} to navigate to
  a viewport further down the viewport tree and \code{seekViewport}
  to navigate to a viewport anywhere else in the tree.

  If a viewport is pushed and it has the same \code{name} as a
  viewport at the same level in the tree, then it replaces the
  existing viewport in the tree.
}
\value{
  \code{downViewport} returns the number of viewports it went down.

  This can be useful for returning to your starting point by doing
  something like \code{depth <- downViewport()} then
  \code{upViewport(depth)}.
}
\author{Paul Murrell}
\seealso{
  \code{\link{viewport}} and \code{\link{vpPath}}.
}
\examples{
# push the same viewport several times
grid.newpage()
vp <- viewport(width=0.5, height=0.5)
pushViewport(vp)
grid.rect(gp=gpar(col="blue"))
grid.text("Quarter of the device",
  y=unit(1, "npc") - unit(1, "lines"), gp=gpar(col="blue"))
pushViewport(vp)
grid.rect(gp=gpar(col="red"))
grid.text("Quarter of the parent viewport",
  y=unit(1, "npc") - unit(1, "lines"), gp=gpar(col="red"))
popViewport(2)
# push several viewports then navigate amongst them
grid.newpage()
grid.rect(gp=gpar(col="grey"))
grid.text("Top-level viewport",
  y=unit(1, "npc") - unit(1, "lines"), gp=gpar(col="grey"))
if (interactive()) Sys.sleep(1.0)
pushViewport(viewport(width=0.8, height=0.7, name="A"))
grid.rect(gp=gpar(col="blue"))
grid.text("1. Push Viewport A",
  y=unit(1, "npc") - unit(1, "lines"), gp=gpar(col="blue"))
if (interactive()) Sys.sleep(1.0)
pushViewport(viewport(x=0.1, width=0.3, height=0.6,
  just="left", name="B"))
grid.rect(gp=gpar(col="red"))
grid.text("2. Push Viewport B (in A)",
  y=unit(1, "npc") - unit(1, "lines"), gp=gpar(col="red"))
if (interactive()) Sys.sleep(1.0)
upViewport(1)
grid.text("3. Up from B to A",
  y=unit(1, "npc") - unit(2, "lines"), gp=gpar(col="blue"))
if (interactive()) Sys.sleep(1.0)
pushViewport(viewport(x=0.5, width=0.4, height=0.8,
  just="left", name="C"))
grid.rect(gp=gpar(col="green"))
grid.text("4. Push Viewport C (in A)",
  y=unit(1, "npc") - unit(1, "lines"), gp=gpar(col="green"))
if (interactive()) Sys.sleep(1.0)
pushViewport(viewport(width=0.8, height=0.6, name="D"))
grid.rect()
grid.text("5. Push Viewport D (in C)",
  y=unit(1, "npc") - unit(1, "lines"))
if (interactive()) Sys.sleep(1.0)
upViewport(0)
grid.text("6. Up from D to top-level",
  y=unit(1, "npc") - unit(2, "lines"), gp=gpar(col="grey"))
if (interactive()) Sys.sleep(1.0)
downViewport("D")
grid.text("7. Down from top-level to D",
  y=unit(1, "npc") - unit(2, "lines"))
if (interactive()) Sys.sleep(1.0)
seekViewport("B")
grid.text("8. Seek from D to B",
  y=unit(1, "npc") - unit(2, "lines"), gp=gpar(col="red"))
pushViewport(viewport(width=0.9, height=0.5, name="A"))
grid.rect()
grid.text("9. Push Viewport A (in B)",
  y=unit(1, "npc") - unit(1, "lines"))
if (interactive()) Sys.sleep(1.0)
seekViewport("A")
grid.text("10. Seek from B to A (in ROOT)",
  y=unit(1, "npc") - unit(3, "lines"), gp=gpar(col="blue"))
if (interactive()) Sys.sleep(1.0)
seekViewport(vpPath("B", "A"))
grid.text("11. Seek from\nA (in ROOT)\nto A (in B)")
popViewport(0)
}
\keyword{dplot}
