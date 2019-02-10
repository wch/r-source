% File src/library/grid/man/grid.path.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{grid.path}
\alias{grid.path}
\alias{pathGrob}
\title{ Draw a Path }
\description{
  These functions create and draw one or more paths.
  The final point of a path will automatically
  be connected to the initial point.
}
\usage{
pathGrob(x, y,
         id=NULL, id.lengths=NULL,
         pathId=NULL, pathId.lengths=NULL,
         rule="winding",
         default.units="npc",
         name=NULL, gp=gpar(), vp=NULL)
grid.path(\dots)
}
\arguments{
  \item{x}{A numeric vector or unit object specifying x-locations.}
  \item{y}{A numeric vector or unit object specifying y-locations.}
  \item{id}{A numeric vector used to separate locations in \code{x} and
    \code{y} into sub-paths.  All locations with the same
    \code{id} belong to the same sub-path.}
  \item{id.lengths}{A numeric vector used to separate locations in \code{x} and
    \code{y} into sub-paths.  Specifies consecutive blocks of
    locations which make up separate sub-paths.}
  \item{pathId}{A numeric vector used to separate locations in \code{x} and
    \code{y} into distinct paths.  All locations with the same
    \code{pathId} belong to the same path.}
  \item{pathId.lengths}{A numeric vector used to separate locations in \code{x} 
    and \code{y} into paths.  Specifies consecutive blocks of locations which 
    make up separate paths.}
  \item{rule}{A character value specifying the fill rule: either
    \code{"winding"} or \code{"evenodd"}.}
  \item{default.units}{A string indicating the default units to use
    if \code{x} or \code{y}
    are only given as numeric vectors.}
  \item{name}{ A character identifier. }
  \item{gp}{An object of class \code{gpar}, typically the output
    from a call to the function \code{gpar}.  This is basically
    a list of graphical parameter settings.}
  \item{vp}{A Grid viewport object (or NULL).}
  \item{\dots}{Arguments passed to \code{pathGrob()}.}
}
\value{
  A grob object.
}
\details{
  Both functions create a path grob (a graphical object describing a
  path), but only \code{grid.path} draws the path (and then only if
  \code{draw} is \code{TRUE}).

  A path is like a polygon except that the former can contain holes, as
  interpreted by the fill rule; these fill a region if the path border
  encircles it an odd or non-zero number of times, respectively.

  Not all graphics devices support this function: for example
  \code{xfig} and \code{pictex} do not.
}
\author{Paul Murrell}
\seealso{
  \link{Grid},
  \code{\link{viewport}}
}
\examples{
pathSample <- function(x, y, rule, gp = gpar()) {
    if (is.na(rule))
        grid.path(x, y, id = rep(1:2, each = 4), gp = gp)
    else
        grid.path(x, y, id = rep(1:2, each = 4), rule = rule, gp = gp)
    if (!is.na(rule))
        grid.text(paste("Rule:", rule), y = 0, just = "bottom")
}

pathTriplet <- function(x, y, title) {
    pushViewport(viewport(height = 0.9, layout = grid.layout(1, 3),
                          gp = gpar(cex = .7)))
    grid.rect(y = 1, height = unit(1, "char"), just = "top",
              gp = gpar(col = NA, fill = "grey"))
    grid.text(title, y = 1, just = "top")
    pushViewport(viewport(layout.pos.col = 1))
    pathSample(x, y, rule = "winding",
               gp = gpar(fill = "grey"))
    popViewport()
    pushViewport(viewport(layout.pos.col = 2))
    pathSample(x, y, rule = "evenodd",
               gp = gpar(fill = "grey"))
    popViewport()
    pushViewport(viewport(layout.pos.col = 3))
    pathSample(x, y, rule = NA)
    popViewport()
    popViewport()
}

pathTest <- function() {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(5, 1)))
    pushViewport(viewport(layout.pos.row = 1))
    pathTriplet(c(.1, .1, .9, .9, .2, .2, .8, .8),
                c(.1, .9, .9, .1, .2, .8, .8, .2),
                "Nested rectangles, both clockwise")
    popViewport()
    pushViewport(viewport(layout.pos.row = 2))
    pathTriplet(c(.1, .1, .9, .9, .2, .8, .8, .2),
                c(.1, .9, .9, .1, .2, .2, .8, .8),
                "Nested rectangles, outer clockwise, inner anti-clockwise")
    popViewport()
    pushViewport(viewport(layout.pos.row = 3))
    pathTriplet(c(.1, .1, .4, .4, .6, .9, .9, .6),
                c(.1, .4, .4, .1, .6, .6, .9, .9),
                "Disjoint rectangles")
    popViewport()
    pushViewport(viewport(layout.pos.row = 4))
    pathTriplet(c(.1, .1, .6, .6, .4, .4, .9, .9),
                c(.1, .6, .6, .1, .4, .9, .9, .4),
                "Overlapping rectangles, both clockwise")
    popViewport()
    pushViewport(viewport(layout.pos.row = 5))
    pathTriplet(c(.1, .1, .6, .6, .4, .9, .9, .4),
                c(.1, .6, .6, .1, .4, .4, .9, .9),
                "Overlapping rectangles, one clockwise, other anti-clockwise")
    popViewport()
    popViewport()
}

pathTest()

# Drawing multiple paths at once
holed_rect <- cbind(c(.15, .15, -.15, -.15, .1, .1, -.1, -.1), 
                    c(.15, -.15, -.15, .15, .1, -.1, -.1, .1))
holed_rects <- rbind(
    holed_rect + matrix(c(.7, .2), nrow = 8, ncol = 2, byrow = TRUE),
    holed_rect + matrix(c(.7, .8), nrow = 8, ncol = 2, byrow = TRUE),
    holed_rect + matrix(c(.2, .5), nrow = 8, ncol = 2, byrow = TRUE)
)
grid.newpage()
grid.path(x = holed_rects[, 1], y = holed_rects[, 2], 
          id = rep(1:6, each = 4), pathId = rep(1:3, each = 8),
          gp = gpar(fill = c('red', 'blue', 'green')),
          rule = 'evenodd')

# Not specifying pathId will treat all points as part of the same path, thus 
# having same fill
grid.newpage()
grid.path(x = holed_rects[, 1], y = holed_rects[, 2], 
          id = rep(1:6, each = 4),
          gp = gpar(fill = c('red', 'blue', 'green')),
          rule = 'evenodd')
}
\keyword{dplot}
