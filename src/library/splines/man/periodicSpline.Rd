% File src/library/splines/man/periodicSpline.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{periodicSpline}
\alias{periodicSpline}
\title{Create a Periodic Interpolation Spline}
\description{
  Create a periodic interpolation spline, either from \code{x} and
  \code{y} vectors, or from a formula/data.frame combination.
}
\usage{
periodicSpline(obj1, obj2, knots, period = 2*pi, ord = 4)
}
\arguments{
 \item{obj1}{either a numeric vector of \code{x} values or a formula.}
 \item{obj2}{if \code{obj1} is numeric this should be a numeric vector
   of the same length.  If \code{obj1} is a formula this can be an
   optional data frame in which to evaluate the names in the formula.}
 \item{knots}{optional numeric vector of knot positions.}
 \item{period}{positive numeric value giving the period for the
   periodic spline.  Defaults to \code{2 * pi}.}
 \item{ord}{integer giving the order of the spline, at least 2.  Defaults
   to 4.  See \code{\link{splineOrder}} for a definition of the order of
   a spline.}
}
\value{
  An object that inherits from class \code{spline}.  The object can be in
  the B-spline representation, in which case it will be a
  \code{pbSpline} object, or in the piecewise polynomial representation
  (a \code{ppolySpline} object).
}
\author{Douglas Bates and Bill Venables}
\seealso{
  \code{\link{splineKnots}},
  \code{\link{interpSpline}}
}
\examples{
require(graphics); require(stats)
xx <- seq( -pi, pi, length.out = 16 )[-1]
yy <- sin( xx )
frm <- data.frame( xx, yy )
pispl <- periodicSpline( xx, yy, period = 2 * pi)
\donttest{pispl}
pispl2 <- periodicSpline( yy ~ xx, frm, period = 2 * pi )
stopifnot(all.equal(pispl, pispl2))  # pispl and pispl2 are the same

plot( pispl )          # displays over one period
points( yy ~ xx, col = "brown")
plot( predict( pispl, seq(-3*pi, 3*pi, length.out = 101) ), type = "l" )
}
\keyword{models}
