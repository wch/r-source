\name{backSpline}
\title{Monotone Inverse Spline}
\usage{
backSpline(object)
}
\alias{backSpline}
\alias{backSpline.nbSpline}
\alias{backSpline.npolySpline}
\arguments{
  \item{object}{
    An object that inherits from class \code{nbSpline} or
    \code{npolySpline}.  That is, the object must represent a natural
    interpolation spline but it can be either in the B-spline
    representation or the piecewise polynomial representation. 
    The spline is checked to see if represents a monotone function.
  }
}
\description{
  Create a monotone inverse of a monotone natural spline.
}
\value{
  An object of class \code{polySpline} that contains the piecewise
  polynomial representation of a function that has the appropriate
  values and derivatives at the knot positions to be an inverse of the
  spline represented by \code{object}.  Technically this object is not a
  spline because the second derivative is not constrained to be
  continuous at the knot positions.  However, it is often a much better
  approximation to the inverse than fitting an interpolation spline to
  the y/x pairs.
}
\author{Douglas Bates and Bill Venables}
\seealso{
  \code{\link{interpSpline}}
}
\examples{
data( women )
ispl <- interpSpline( women$height, women$weight )
bspl <- backSpline( ispl )
plot( bspl )                   # plots over the range of the knots
points( women$weight, women$height )
}
\keyword{ models }
