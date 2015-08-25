% File src/library/splines/man/xyVector.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{xyVector}
\alias{xyVector}
\title{Construct an \code{xyVector} Object}
\description{
  Create an object to represent a set of x-y pairs.  The resulting
  object can be treated as a matrix or as a data frame or as a vector.
  When treated as a vector it reduces to the \code{y} component only.

  The result of functions such as \code{predict.spline} is returned as
  an \code{xyVector} object so the x-values used to generate the
  y-positions are retained, say for purposes of generating plots.
}
\usage{
xyVector(x, y)
}
\arguments{
  \item{x}{a numeric vector}
  \item{y}{a numeric vector of the same length as \code{x}}
}
\value{
  An object of class \code{xyVector} with components
  \item{x}{a numeric vector}
  \item{y}{a numeric vector of the same length as \code{x}}
}
\author{Douglas Bates and Bill Venables}
\examples{
require(stats); require(graphics)
ispl <- interpSpline( weight ~ height, women )
weights <- predict( ispl, seq( 55, 75, length.out = 51 ))
class( weights )
plot( weights, type = "l", xlab = "height", ylab = "weight" )
points( women$height, women$weight )
weights
}
\keyword{ models }
