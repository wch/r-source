\name{grid.lines}
\alias{grid.lines}
\alias{linesGrob}
\title{Draw Lines in a Grid Viewport}
\description{
  These functions create and draw a series of lines.
}
\usage{
grid.lines(x = unit(c(0, 1), "npc", units.per.obs),
       y = unit(c(0, 1), "npc", units.per.obs), 
       default.units = "npc", units.per.obs = FALSE,
       arrow = NULL, name = NULL,
       gp=gpar(), draw = TRUE, vp = NULL)
linesGrob(x = unit(c(0, 1), "npc", units.per.obs),
       y = unit(c(0, 1), "npc", units.per.obs), 
       default.units = "npc", units.per.obs = FALSE,
       arrow = NULL, name = NULL,
       gp=gpar(), vp = NULL)
}
\arguments{
  \item{x}{A numeric vector or unit object specifying x-values.}
  \item{y}{A numeric vector or unit object specifying y-values.}
  \item{default.units}{A string indicating the default units to use
    if \code{x} or \code{y} are only given as numeric vectors.}  
  \item{units.per.obs}{A logical value to indicate whether each
    individual (x, y) location has its own unit(s) specified.}
  \item{arrow}{A list describing arrow heads to place at either end
    of the line, as produced by the \code{arrow} function.}
  \item{name}{ A character identifier. }
  \item{gp}{An object of class \code{gpar}, typically the output
    from a call to the function \code{gpar}.  This is basically
    a list of graphical parameter settings.}
  \item{draw}{A logical value indicating whether graphics output
    should be produced.}
  \item{vp}{A Grid viewport object (or NULL).}
}
\details{
  Both functions create a lines grob (a graphical object describing
  lines), but only \code{grid.lines}
  draws the lines (and then only if \code{draw} is \code{TRUE}).
  
}
\value{
  A lines grob.  \code{grid.lines} returns the value invisibly.
}
\author{Paul Murrell}
\seealso{
  \link{Grid},
  \code{\link{viewport}},
  \code{\link{arrow}}}
}
\keyword{dplot}
