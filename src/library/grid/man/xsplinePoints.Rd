% File src/library/grid/man/viewport.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2009 R Core Team
% Distributed under GPL 2 or later

\name{xsplinePoints}
\alias{xsplinePoints}
\alias{bezierPoints}
\title{
  Return the points that would be used to draw an Xspline
  (or a Bezier curve).
}
\description{
  Rather than drawing an Xspline (or Bezier curve), this function returns
  the points that would be used to draw the series of line
  segments for the Xspline.  This may be useful to post-process
  the Xspline curve, for example, to clip the curve.
}
\usage{
xsplinePoints(x)
bezierPoints(x)
}
\arguments{
  \item{x}{
    An Xspline grob, as produced by the \code{xsplineGrob()} function
    (or a beziergrob, as produced by the \code{bezierGrob()} function).
  }
}
\details{
  The points returned by this function will only be relevant
  for the drawing context in force when this function was called.
}
\value{
  Depends on how many Xsplines would be drawn.  If only one, then
  a list with two components, named x and y, both of which are
  unit objects (in inches).  If several Xsplines would be drawn
  then the result of this function is a list of lists.
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{xsplineGrob}} and
  \code{\link{bezierGrob}}
}
\examples{
grid.newpage()
xsg <- xsplineGrob(c(.1, .1, .9, .9), c(.1, .9, .9, .1), shape=1)
grid.draw(xsg)
trace <- xsplinePoints(xsg)
grid.circle(trace$x, trace$y, default.units="inches", r=unit(.5, "mm"))

grid.newpage()
vp <- viewport(width=.5)
xg <- xsplineGrob(x=c(0, .2, .4, .2, .5, .7, .9, .7),
                  y=c(.5, 1, .5, 0, .5, 1, .5, 0),
                  id=rep(1:2, each=4),
                  shape=1,
                  vp=vp)
grid.draw(xg)
trace <- xsplinePoints(xg)
pushViewport(vp)
invisible(lapply(trace, function(t) grid.lines(t$x, t$y, gp=gpar(col="red"))))
popViewport()

grid.newpage()
bg <- bezierGrob(c(.2, .2, .8, .8), c(.2, .8, .8, .2))
grid.draw(bg)
trace <- bezierPoints(bg)
grid.circle(trace$x, trace$y, default.units="inches", r=unit(.5, "mm"))
}
\keyword{ dplot }
