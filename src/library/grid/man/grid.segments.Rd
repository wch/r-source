% File src/library/grid/man/grid.segments.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{grid.segments}
\alias{grid.segments}
\alias{segmentsGrob}
\title{ Draw Line Segments }
\description{
  These functions create and draw line segments.
}
\usage{
grid.segments(x0 = unit(0, "npc"), y0 = unit(0, "npc"),
              x1 = unit(1, "npc"), y1 = unit(1, "npc"),
              default.units = "npc",
              arrow = NULL,
              name = NULL, gp = gpar(), draw = TRUE, vp = NULL)
segmentsGrob(x0 = unit(0, "npc"), y0 = unit(0, "npc"),
              x1 = unit(1, "npc"), y1 = unit(1, "npc"),
              default.units = "npc",
              arrow = NULL, name = NULL, gp = gpar(), vp = NULL)
}
\arguments{
  \item{x0}{ Numeric indicating the starting x-values of the line segments. }
  \item{y0}{ Numeric indicating the starting y-values of the line
    segments. }
  \item{x1}{ Numeric indicating the stopping x-values of the line
    segments. }
  \item{y1}{ Numeric indicating the stopping y-values of the line segments.}
  \item{default.units}{ A string. }
  \item{arrow}{A list describing arrow heads to place at either end
    of the line segments, as produced by the \code{arrow} function.}
  \item{name}{ A character identifier. }
  \item{gp}{ An object of class \code{gpar}. }
  \item{draw}{ A logical value indicating whether graphics output
    should be produced.}
  \item{vp}{A Grid viewport object (or NULL).}
}
\details{
  Both functions create a segments grob (a graphical object describing
  segments), but only \code{grid.segments}
  draws the segments (and then only if \code{draw} is \code{TRUE}).

}
\value{
  A segments grob.  \code{grid.segments} returns the value invisibly.
}
\author{Paul Murrell}
\seealso{
  \link{Grid},
  \code{\link{viewport}},
  \code{\link{arrow}}
}
\keyword{dplot}
