\name{grid.lines}
\alias{grid.lines}
\alias{linesGrob}
\alias{grid.polyline}
\alias{polylineGrob}
\title{Draw Lines in a Grid Viewport}
\description{
  These functions create and draw a series of lines.
}
\usage{
grid.lines(x = unit(c(0, 1), "npc"),
          y = unit(c(0, 1), "npc"), 
          default.units = "npc", 
          arrow = NULL, name = NULL,
          gp=gpar(), draw = TRUE, vp = NULL)
linesGrob(x = unit(c(0, 1), "npc"),
          y = unit(c(0, 1), "npc"), 
          default.units = "npc", 
          arrow = NULL, name = NULL,
          gp=gpar(), vp = NULL)
grid.polyline(...)
polylineGrob(x = unit(c(0, 1), "npc"),
             y = unit(c(0, 1), "npc"), 
             id=NULL, id.lengths=NULL,
             default.units = "npc", 
             arrow = NULL, name = NULL,
             gp=gpar(), vp = NULL)
}
\arguments{
  \item{x}{A numeric vector or unit object specifying x-values.}
  \item{y}{A numeric vector or unit object specifying y-values.}
  \item{default.units}{A string indicating the default units to use
    if \code{x} or \code{y} are only given as numeric vectors.}  
  \item{arrow}{A list describing arrow heads to place at either end
    of the line, as produced by the \code{arrow} function.}
  \item{name}{ A character identifier. }
  \item{gp}{An object of class \code{gpar}, typically the output
    from a call to the function \code{gpar}.  This is basically
    a list of graphical parameter settings.}
  \item{draw}{A logical value indicating whether graphics output
    should be produced.}
  \item{vp}{A Grid viewport object (or NULL).}
  \item{id}{A numeric vector used to separate locations in \code{x} and
    \code{y} into multiple lines.  All locations with the same
    \code{id} belong to the same line.}
  \item{id.lengths}{A numeric vector used to separate locations in \code{x} and
    \code{y} into multiple lines.  Specifies consecutive blocks of
    locations which make up separate lines.}
  \item{...}{Arguments passed to \code{polylineGrob}.}
}
\details{
  The first two functions create a lines grob (a graphical object describing
  lines), and \code{grid.lines}
  draws the lines (if \code{draw} is \code{TRUE}).

  The second two functions create or draw a polyline grob,
  which is just like a lines grob, except that there can be
  multiple distinct lines drawn.
}
\value{
  A lines grob or a polyline grob.
  \code{grid.lines} returns a lines grob invisibly.
}
\author{Paul Murrell}
\seealso{
  \link{Grid},
  \code{\link{viewport}},
  \code{\link{arrow}}
}
\examples{
grid.lines()
# Using id (NOTE: locations are not in consecutive blocks)
grid.newpage()
grid.polyline(x=c((0:4)/10, rep(.5, 5), (10:6)/10, rep(.5, 5)),
             y=c(rep(.5, 5), (10:6/10), rep(.5, 5), (0:4)/10),
             id=rep(1:5, 4),
             gp=gpar(col=1:5, lwd=3))
# Using id.lengths
grid.newpage()
grid.polyline(x=outer(c(0, .5, 1, .5), 5:1/5),
             y=outer(c(.5, 1, .5, 0), 5:1/5),
             id.lengths=rep(4, 5),
             gp=gpar(col=1:5, lwd=3))
}
\keyword{dplot}
