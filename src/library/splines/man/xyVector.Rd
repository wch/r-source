%% $Id: xyVector.Rd,v 1.5.4.1 2001/04/03 16:57:43 ripley Exp $
\name{xyVector}
\title{Construct an xyVector Object}
\usage{
xyVector(x, y)
}
\alias{xyVector}
\alias{as.data.frame.xyVector}
\alias{plot.xyVector}
\arguments{
 \item{x}{a numeric vector}
 \item{y}{a numeric vector of the same length as \code{x}}
}
\description{
  Create an object to represent a set of x-y pairs.  The resulting
  object can be treated as a matrix or as a data frame or as a vector.
  When treated as a vector it reduces to the \code{y} component only.

  The result of functions such as \code{predict.spline} is returned as
  an \code{xyVector} object so the x-values used to generate the
  y-positions are retained, say for purposes of generating plots.
}
\value{
  An object of class \code{xyVector} with components
  \item{x}{a numeric vector}
  \item{y}{a numeric vector of the same length as \code{x}}
}
\author{Douglas Bates and Bill Venables}
\examples{
data( women )
ispl <- interpSpline( weight ~ height, women )
weights <- predict( ispl, seq( 55, 75, len = 51 ))
class( weights )
plot( weights, type = "l", xlab = "height", ylab = "weight" )
points( women$height, women$weight )
weights
}
\keyword{ models }%-- one or more ...
