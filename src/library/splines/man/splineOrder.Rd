\name{splineOrder}
\title{Determine the Order of a Spline}
\usage{
splineOrder(object)
}
\alias{splineOrder}
\alias{splineOrder.bSpline}
\alias{splineOrder.polySpline}
\arguments{
 \item{object}{An object that inherits from class \code{spline}.}
}
\description{
  Return the order of the object.  The order of a spline is the number
  of coefficients in each piece of the piecewise polynomial
  representation.  Thus a cubic spline has order 4.
}
\value{
  An integer vector of length 1.
}
\author{Douglas Bates and Bill Venables}
\seealso{
  \code{\link{splineKnots}},
  \code{\link{interpSpline}},
  \code{\link{periodicSpline}}}}
\examples{
library( splines )
data( women )
splineOrder( interpSpline( weight ~ height, women ) )
}
\keyword{ models }

