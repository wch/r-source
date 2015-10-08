% File src/library/splines/man/interpSpline.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{interpSpline}
\alias{interpSpline}
\title{Create an Interpolation Spline}
\description{
  Create an interpolation spline, either from \code{x} and \code{y}
  vectors (\code{default} method), or from a \code{formula} / \code{data.frame}
  combination (\code{formula} method).
}
\usage{
interpSpline(obj1, obj2, bSpline = FALSE, period = NULL,
             na.action = na.fail, sparse = FALSE)
}
\arguments{
 \item{obj1}{either a numeric vector of \code{x} values or a formula.}
 \item{obj2}{if \code{obj1} is numeric this should be a numeric vector
   of the same length.  If \code{obj1} is a formula this can be an
   optional data frame in which to evaluate the names in the formula.}
 \item{bSpline}{if \code{TRUE} the b-spline representation is returned,
   otherwise the piecewise polynomial representation is returned.
   Defaults to \code{FALSE}.}
 \item{period}{an optional positive numeric value giving a period for a
   periodic interpolation spline.}
 \item{na.action}{a optional function which indicates what should happen
   when the data contain \code{NA}s.  The default action
   (\code{na.omit}) is to omit any incomplete observations.  The
   alternative action \code{na.fail} causes \code{interpSpline} to print
   an error message and terminate if there are any incomplete
   observations.}
 \item{sparse}{logical indicating if the result should be a
   \code{"sparseMatrix"}, see \code{\link{splineDesign}} on which
   \code{interpSpline()} builds.}
}
\value{
  An object that inherits from (S3) class \code{spline}. The object can be in
  the B-spline representation, in which case it will be of class
  \code{nbSpline} for natural B-spline, or in the piecewise polynomial
  representation, in which case it will be of class \code{npolySpline}.
}
\author{Douglas Bates and Bill Venables}
\seealso{
  \code{\link{splineKnots}},
  \code{\link{splineOrder}},
  \code{\link{periodicSpline}}.
}
\examples{
require(graphics); require(stats)
ispl <- interpSpline( women$height, women$weight )
ispl2 <- interpSpline( weight ~ height,  women )
# ispl and ispl2 should be the same
plot( predict( ispl, seq( 55, 75, length.out = 51 ) ), type = "l" )
points( women$height, women$weight )
plot( ispl )    # plots over the range of the knots
points( women$height, women$weight )
splineKnots( ispl )
}
\keyword{ models }
