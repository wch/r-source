% File src/library/grid/man/patterns.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{patterns}
\alias{patterns}
\alias{linearGradient}
\alias{radialGradient}
\alias{pattern}
\title{ Define Gradient and Pattern Fills }
\description{
  Functions to define gradient fills and pattern fills.
}
\usage{
linearGradient(colours = c("black", "white"),
               stops = seq(0, 1, length.out = length(colours)),
               x1 = unit(0, "npc"), y1 = unit(0, "npc"),  
               x2 = unit(1, "npc"), y2 = unit(1, "npc"),
               default.units = "npc",
               extend = c("pad", "repeat", "reflect", "none"),
               group = TRUE) 
radialGradient(colours = c("black", "white"),
               stops = seq(0, 1, length.out = length(colours)),
               cx1 = unit(.5, "npc"), cy1 = unit(.5, "npc"),
               r1 = unit(0, "npc"),
               cx2 = unit(.5, "npc"), cy2 = unit(.5, "npc"),
               r2 = unit(.5, "npc"),
               default.units = "npc",
               extend = c("pad", "repeat", "reflect", "none"),
               group = TRUE) 
pattern(grob,
        x = 0.5, y = 0.5, width = 1, height = 1,
        default.units = "npc",
        just="centre", hjust=NULL, vjust=NULL,
        extend = c("pad", "repeat", "reflect", "none"),
        gp = gpar(fill="transparent"),
        group = TRUE) 
}
\arguments{
  \item{colours}{Two or more colours for the gradient to transition between.}
  \item{stops}{Locations of the gradient colours between the start and end
    points of the gradient (as a proportion of the distance from the
    start point to the end point).}
  \item{x1, y1, x2, y2}{The start and end points for a linear gradient.}
  \item{default.units}{The coordinate system
    to use if any location or dimension is
    specified as just a numeric value.}
  \item{extend}{What happens outside the start and end of the gradient
    (see Details).}
  \item{cx1, cy1, r1, cx2, cy2, r2}{The centre and radius of the start
    and end circles for a radial gradient.}
  \item{grob}{A grob (or a \I{gTree}) that will be drawn as the tile in
    a pattern fill.}
  \item{x, y, width, height}{The size of the tile for a pattern fill.}
  \item{just, hjust, vjust}{The justification of the tile relative to
    its location.}
  \item{gp}{Default graphical parameter settings for the tile.}
  \item{group}{A logical indicating whether the gradient or pattern is
    relative to the bounding box of the grob or whether
    it is relative to individual shapes within the grob.}
}
\details{
  Use these functions to define a gradient fill or pattern fill and
  then use the resulting object as the value for \code{fill}
  in a call to the \code{gpar()} function.

  The possible values of extend, and their meanings, are:
  \itemize{
    \item[\code{pad}:] propagate the value of the gradient at its
    boundary.
    \item[\code{none}:] produce no fill beyond the limits of the
    gradient.
    \item[\code{repeat}:] repeat the fill.
    \item[\code{reflect}:] repeat the fill in reverse.
  }

  To create a tiling pattern, provide a simple grob (like a circle),
  specify the location and size of the pattern to include the simple
  grob, and specify \code{extend="repeat"}.

  On viewports, gradients and patterns are relative to the entire
  viewport, unless \code{group = FALSE}, in which case they are
  relative to individual grobs as they are drawn.
  On \I{gTree}s, gradients and patterns are relative to a bounding
  box around all of the children of the \I{gTree},
  unless \code{group = FALSE}, in which case they are
  relative to individual children as they are drawn.
  On grobs, gradients and patterns are relative to a bounding
  box around all of the shapes that are drawn by the grob,
  unless \code{group = FALSE}, in which case they are
  relative to individual shapes.
}
\section{Warning}{
  Gradient fills and pattern fills are not supported on all graphics
  devices.  Where they are not supported, closed shapes will be
  rendered with a transparent fill.  Where they are supported,
  not all values of \code{extend} are supported.

  On Cairo devices, use of clipping in the pattern definition should be
  avoided because it is very likely to result in distortion of the pattern tile.
}
\value{
  A linear gradient or radial gradient or pattern object.
}
\author{Paul Murrell}
\seealso{
  \code{\link{gpar}}
}
\keyword{dplot}
