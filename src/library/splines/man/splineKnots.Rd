% File src/library/splines/man/splineKnots.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{splineKnots}
\alias{splineKnots}
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
ispl <- interpSpline( weight ~ height, women )
splineKnots( ispl )
}
\keyword{ models }
