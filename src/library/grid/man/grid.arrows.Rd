\name{grid.arrows}
\alias{grid.arrows}
\title{ Draw Arrows }
\description{
  This will draw arrows at either end of a line or an existing
  line.to, lines, or segments grob.
}
\usage{
grid.arrows(x = c(0.25, 0.75), y = 0.5, default.units = "npc",
            grob = NULL,
            angle = 30, length = unit(0.25, "inches"),
            ends = "last", type = "open",
            gp = gpar(), draw = TRUE, vp = NULL)
}
\arguments{
  \item{x}{A numeric vector or unit object specifying x-values.}
  \item{y}{A numeric vector or unit object specifying y-values.}
  \item{default.units}{A string indicating the default units to use
    if \code{x} or \code{y} are only given as numeric vectors.}  
  \item{grob}{ A grob to add arrows to;  currently can only be
    a line.to, lines, or segments grob. }
  \item{angle}{ A numeric specifying (half) the width of the arrow head
    (in degrees). }
  \item{length}{ A unit object specifying the length of the arrow head. }
  \item{ends}{ One of \code{"first"}, \code{"last"}, or \code{"both"},
    indicating which end of the line to add arrow heads. }
  \item{type}{ Either \code{"open"} or \code{"closed"} to indicate
    the type of arrow head.   }
  \item{gp}{An object of class \code{gpar}, typically the output
    from a call to the function \code{gpar}.  This is basically
    a list of graphical parameter settings.}
  \item{draw}{A logical value indicating whether graphics output
    should be produced.}
  \item{vp}{A Grid viewport object (or NULL).}
}
\details{
  If the grob argument is specified, this overrides any x
  and/or y arguments.
}
\value{
  An object of class \code{"grob"}.
}
\author{ Paul Murrell }
\seealso{
  \link{Grid},
  \code{\link{viewport}},
  \code{\link{grid.line.to}},
  \code{\link{grid.lines}},
  \code{\link{grid.segments}}
}
\examples{
push.viewport(viewport(layout=grid.layout(2, 4)))
push.viewport(viewport(layout.pos.col=1,
                       layout.pos.row=1))
grid.rect(gp=gpar(col="grey"))
grid.arrows()
pop.viewport()
push.viewport(viewport(layout.pos.col=2,
                       layout.pos.row=1))
grid.rect(gp=gpar(col="grey"))
grid.arrows(angle=15, type="closed")
pop.viewport()
push.viewport(viewport(layout.pos.col=3,
                       layout.pos.row=1))
grid.rect(gp=gpar(col="grey"))
grid.arrows(angle=5, length=unit(0.1, "npc"),
            type="closed", gp=gpar(fill="white"))
pop.viewport()
push.viewport(viewport(layout.pos.col=4,
                       layout.pos.row=1))
grid.rect(gp=gpar(col="grey"))
grid.arrows(x=unit(0:80/100, "npc"),
            y=unit(1 - (0:80/100)^2, "npc"))
pop.viewport()
push.viewport(viewport(layout.pos.col=1,
                       layout.pos.row=2))
grid.rect(gp=gpar(col="grey"))
grid.arrows(ends="both")
pop.viewport()
push.viewport(viewport(layout.pos.col=2,
                       layout.pos.row=2))
grid.rect(gp=gpar(col="grey"))
# Recycling arguments
grid.arrows(x=unit(1:10/11, "npc"), y=unit(1:3/4, "npc"))
pop.viewport()
push.viewport(viewport(layout.pos.col=3,
                       layout.pos.row=2))
grid.rect(gp=gpar(col="grey"))
gs <- grid.segments(x0=unit(1:4/5, "npc"),
                    x1=unit(1:4/5, "npc"))
grid.arrows(grob=gs, length=unit(0.1, "npc"),
            type="closed", gp=gpar(fill="white"))
pop.viewport()
push.viewport(viewport(layout.pos.col=4,
                       layout.pos.row=2))
grid.rect(gp=gpar(col="grey"))
gl <- grid.lines(x=unit(0:80/100, "npc"),
                 y=unit((0:80/100)^2, "npc"))
grid.arrows(grob=gl, angle=15, type="closed", gp=gpar(fill="black"))
pop.viewport()
push.viewport(viewport(layout.pos.col=1,
                       layout.pos.row=2))
grid.move.to(x=0.5, y=0.8)
pop.viewport()
push.viewport(viewport(layout.pos.col=4,
                       layout.pos.row=1))
glt <- grid.line.to(x=0.5, y=0.2, gp=gpar(lwd=3))
grid.arrows(grob=glt, ends="first", gp=gpar(lwd=3))
pop.viewport(2)
grid.edit(gl, y=unit((0:80/100)^3, "npc"))
}
\keyword{dplot}
