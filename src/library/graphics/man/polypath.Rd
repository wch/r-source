% File src/library/graphics/man/path.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{polypath}
\alias{polypath}
\title{Path Drawing}
\description{
  \code{path} draws a path whose vertices are
  given in \code{x} and \code{y}.
}
\usage{
polypath(x, y = NULL, 
         border = NULL, col = NA, lty = par("lty"),
         rule = "winding", \dots)
}
\arguments{
  \item{x,y}{vectors containing the coordinates of the vertices
    of the path.}
   \item{col}{the color for filling the path. The default,
     \code{NA}, is to leave paths unfilled, unless \code{density} is
     specified.  (For back-compatibility, \code{NULL} is equivalent to
     \code{NA}.)  If \code{density} is specified with a positive value
     this gives the color of the shading lines.}
  \item{border}{the color to draw the border.  The default, \code{NULL},
    means to use \code{\link{par}("fg")}.  Use \code{border = NA} to
    omit borders.
    
    For compatibility with S, \code{border} can also be logical, in
    which case \code{FALSE} is equivalent to \code{NA} (borders omitted)
    and \code{TRUE} is equivalent to \code{NULL} (use the foreground colour),
  }
  \item{lty}{the line type to be used, as in \code{\link{par}}.}
  \item{rule}{character value specifying the path fill mode: either
    \code{"winding"} or \code{"evenodd"}.} 
  \item{\dots}{graphical parameters such as \code{xpd}, \code{lend},
    \code{ljoin} and \code{lmitre} can be given as arguments.}
}
\details{
  The coordinates can be passed in a plotting structure
  (a list with \code{x} and \code{y} components), a two-column matrix,
  \dots.  See \code{\link{xy.coords}}.

  It is assumed that the path is to be closed by joining the last point to
  the first point.
 
  The coordinates can contain missing values.  The behaviour is similar
  to that of \code{\link{polygon}}, except
  that instead of breaking a polygon into several polygons, \code{NA}
  values break the path into several sub-paths
  (including closing the last point to the first point in each
  sub-path).  See
  the examples below.

  The distinction between a path and a polygon is that the former
  can contain holes, as interpreted by the fill rule; these fill a region if
  the path border encircles it an odd or non-zero number of times,
  respectively.
  
  Hatched shading (as implemented for \code{polygon()}) is not
  (currently) supported.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Murrell, P. (2005) \emph{R Graphics}. Chapman & Hall/CRC Press.
}
\seealso{
  \code{\link{segments}} for even more flexibility, \code{\link{lines}},
  \code{\link{rect}}, \code{\link{box}}, \code{\link{polygon}}.

  \code{\link{par}} for how to specify colors.
}
\examples{
plotPath <- function(x, y, col="grey", rule="winding") {
    plot.new()
    plot.window(range(x, na.rm=TRUE), range(y, na.rm=TRUE))
    polypath(x, y, col=col, rule=rule)
    if (!is.na(col))
        mtext(paste("Rule:", rule), side=1, line=0)
}

plotRules <- function(x, y, title) {
    plotPath(x, y)
    plotPath(x, y, rule="evenodd")
    mtext(title, side=3, line=0)
    plotPath(x, y, col=NA)
}

op <- par(mfrow=c(5, 3), mar=c(2, 1, 1, 1))

plotRules(c(.1, .1, .9, .9, NA, .2, .2, .8, .8),
          c(.1, .9, .9, .1, NA, .2, .8, .8, .2),
          title="Nested rectangles, both clockwise")
plotRules(x=c(.1, .1, .9, .9, NA, .2, .8, .8, .2),
          y=c(.1, .9, .9, .1, NA, .2, .2, .8, .8),
          title="Nested rectangles, outer clockwise, inner anti-clockwise")
plotRules(x=c(.1, .1, .4, .4, NA, .6, .9, .9, .6),
          y=c(.1, .4, .4, .1, NA, .6, .6, .9, .9),
          title="Disjoint rectangles")
plotRules(x=c(.1, .1, .6, .6, NA, .4, .4, .9, .9),
          y=c(.1, .6, .6, .1, NA, .4, .9, .9, .4),
          title="Overlapping rectangles, both clockwise")
plotRules(x=c(.1, .1, .6, .6, NA, .4, .9, .9, .4),
          y=c(.1, .6, .6, .1, NA, .4, .4, .9, .9),
          title="Overlapping rectangles, one clockwise, other anti-clockwise")

par(op)
}
\keyword{aplot}
