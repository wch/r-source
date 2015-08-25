% File src/library/splines/man/polySpline.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{polySpline}
\alias{polySpline}
\alias{as.polySpline}
\title{Piecewise Polynomial Spline Representation}
\description{
  Create the piecewise polynomial representation of a spline object.
}
\usage{
polySpline(object, \dots)
as.polySpline(object, \dots)
}
\arguments{
 \item{object}{An object that inherits from class \code{spline}.}
 \item{\dots}{Optional additional arguments.  At present no additional
     arguments are used.}
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
require(graphics)
ispl <- polySpline(interpSpline( weight ~ height,  women, bSpline = TRUE))
print( ispl )   # print the piecewise polynomial representation
plot( ispl )    # plots over the range of the knots
points( women$height, women$weight )
}
\keyword{ models }
