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
  Return the order of a spline object.
}
\details{
  The order of a spline is the number
  of coefficients in each piece of the piecewise polynomial
  representation.  Thus a cubic spline has order 4.
}
\value{
  A positive integer.
}
\author{Douglas Bates and Bill Venables}
\seealso{
  \code{\link{splineKnots}},
  \code{\link{interpSpline}},
  \code{\link{periodicSpline}}}}
\examples{
data( women )
splineOrder( interpSpline( weight ~ height, women ) )
}
\keyword{ models }

