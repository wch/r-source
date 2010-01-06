% File src/library/grid/man/viewport.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2009 R Core Development Team
% Distributed under GPL 2 or later

\name{xsplinePoints}
\alias{xsplinePoints}
\title{
  Return the points that would be used to draw an Xspline.
}
\description{
  Rather than drawing an Xspline, this function returns
  the points that would be used to draw the series of line
  segments for the Xspline.  This may be useful to post-process
  the Xspline curve, for example, to clip the curve.
}
\usage{
xsplinePoints(x)
}
\arguments{
  \item{x}{
    An Xspline grob, as produced by the \code{xsplineGrob()} function.
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
  \code{\link{xsplineGrob}}
}
\examples{
grid.newpage()
xsg <- xsplineGrob(c(.1, .1, .9, .9), c(.1, .9, .9, .1), shape=1)
grid.draw(xsg)
trace <- xsplinePoints(xsg)
grid.circle(trace$x, trace$y, default.units="inches", r=unit(.5, "mm"))
}
\keyword{ dplot }
