\name{polySpline}
\title{Piecewise Polynomial Spline Representation}
\usage{
polySpline(object, \dots)
as.polySpline(object, \dots)
}
\alias{polySpline}
\alias{polySpline.bSpline}
\alias{polySpline.nbSpline}
\alias{polySpline.pbSpline}
\alias{polySpline.polySpline}
\alias{as.polySpline}
\alias{plot.polySpline}
\alias{predict.polySpline}
\alias{print.polySpline}
\arguments{
 \item{object}{An object that inherits from class \code{spline}.}
 \item{\dots}{Optional additional arguments.  At present no additional
     arguments are used.}
}
\description{
  Create the piecewise polynomial representation of a spline object.
}
\value{
  An object that inherits from class \code{polySpline}.  This is the
  piecewise polynomial representation of a univariate spline function.
  It is defined by a set of distinct numeric values called knots.  The
  spline function is a polynomial function between each successive pair
  of knots.  At each interior knot the polynomial segments on each side
  are constrained to have the same value of the function and some of its
  derivatives.
}
\author{Douglas Bates and Bill Venables}
\seealso{
  \code{\link{interpSpline}},
  \code{\link{periodicSpline}},
  \code{\link{splineKnots}},
  \code{\link{splineOrder}}
}
\examples{
library( splines )
data( women )
ispl <- polySpline( interpSpline( weight ~ height,  women ) )
print( ispl )   # print the piecewise polynomial representation
plot( ispl )    # plots over the range of the knots
points( women$weight, women$height )
}
\keyword{ models }
