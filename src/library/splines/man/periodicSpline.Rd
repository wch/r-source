\name{periodicSpline}
\title{Create a Periodic Interpolation Spline}
\usage{
periodicSpline(obj1, obj2, knots, period, ord)
}
\alias{periodicSpline}
\alias{periodicSpline.default}
\alias{periodicSpline.formula}
\arguments{
 \item{obj1}{Either a numeric vector of \code{x} values or a formula.}
 \item{obj2}{If \code{obj1} is numeric this should be a numeric vector
   of the same length.  If \code{obj1} is a formula this can be an
   optional data frame in which to evaluate the names in the formula.}
 \item{knots}{An optional numeric vector of knot positions.}
 \item{period}{A positive numeric value giving the period for the
   periodic spline.  Defaults to \code{2 * pi}.}
 \item{ord}{A positive integer giving the order of the spline.  Defaults
   to 4.  See \code{?splineOrder} for a definition of the order of a spline.}
}
\description{
  Create a periodic interpolation spline, either from \code{x} and
  \code{y} vectors, or from a formula/data.frame combination.
}
\value{
  An object that inherits from class \code{spline}. The object can be in
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
xx <- seq( -pi, pi, len = 16 )[-1]
yy <- sin( xx )
frm <- data.frame( xx, yy )
print( pispl <- periodicSpline( xx, yy, period = 2 * pi ) )
print( pispl2 <- periodicSpline( yy ~ xx, frm, period = 2 * pi ) )
# pispl and pispl2 should be the same
plot( predict( pispl, seq(-3*pi, 3*pi, len = 101) ), type = "l" )
plot( pispl )          # displays over one period
}
\keyword{ models }
