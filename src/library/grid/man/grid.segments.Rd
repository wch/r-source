\name{grid.segments}
\alias{grid.segments}
\title{ Draw Line Segments in a Grid Viewport }
\description{
  This function draws line segments.
}
\usage{
grid.segments(x0 = unit(0, "npc"), y0 = unit(0, "npc"),
              x1 = unit(1, "npc"), y1 = unit(1, "npc"),
              default.units = "npc", units.per.obs = FALSE,
              gp = gpar(), draw = TRUE, vp = NULL)
}
\arguments{
  \item{x0}{ Numeric indicating the starting x-values of the line segments. }
  \item{y0}{ Numeric indicating the starting y-values of the line
    segments. }
  \item{x1}{ Numeric indicating the stopping x-values of the line
    segments. }
  \item{y1}{ Numeric indicating the stopping y-values of the line segments.}
  \item{gp}{ An object of class \code{gpar}. }
  \item{default.units}{ A string. }
  \item{units.per.obs}{ A boolean indicating whether distinct units are
    given for each x/y-value. }
  \item{draw}{ A logical value indicating whether graphics output
    should be produced.}
  \item{vp}{A Grid viewport object (or NULL)}
}
\value{
  An object of class \code{"grob"}.
}
\author{ Paul Murrell                }
\seealso{
  \link{Grid},
  \code{\link{viewport}}
}
\keyword{dplot}
