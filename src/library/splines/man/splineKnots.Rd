%% $Id: splineKnots.Rd,v 1.3 2001/08/25 22:17:44 hornik Exp $
\name{splineKnots}
\alias{splineKnots}
\alias{splineKnots.spline}
\title{Knot Vector from a Spline}
\description{
  Return the knot vector corresponding to a spline object.
}
\usage{
splineKnots(object)
}
\arguments{
  \item{object}{an object that inherits from class \code{"spline"}.}
}
\value{
  A non-decreasing numeric vector of knot positions.
}
\author{Douglas Bates and Bill Venables}
\examples{
data( women )
ispl <- interpSpline( weight ~ height, women )
splineKnots( ispl )
}
\keyword{ models }
