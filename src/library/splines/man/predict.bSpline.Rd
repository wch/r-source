\name{predict.bSpline}
\alias{predict.bSpline}
\alias{predict.nbSpline}
\alias{predict.pbSpline}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~}
\description{
Evaluate a bSpline at new values of x
}
\usage{
predict.bSpline(object, x, nseg=50, deriv=0)
}
\arguments{
 \item{object}{An object that inherits from the \code{bSpline} class.}
 \item{x}{A numeric vector of \code{x} values at which to evaluate the
     spline.  If this argument is missing a suitable set of \code{x}
     values is generated as a sequence of \code{nseq} segments spanning
     the range of the knots.}
 \item{nseg}{A positive integer giving the number of segments in a set
     of equally-spaced \code{x} values spanning the range of the knots
     in \code{object}.  This value is only used if \code{x} is missing.}
 \item{deriv}{An integer between 0 and \code{splineOrder(object) - 1}
     specifying the derivative to evaluate.}
}
\value{
  an \code{xyVector} with components
  \item{x}{the supplied or inferred numeric vector of \code{x} values}
  \item{y}{the value of the spline (or its \code{deriv}'th derivative)
      at the \code{x} vector}
}
\author{Douglas Bates and William Venables}
\seealso{\code{\link{xyVector}}, \code{\link{interpSpline}},
    \code{\link{periodicSpline}}}
\examples{
library( splines )
data( women )
ispl <- interpSpline( weight ~ height,  women )
opar <- par(mfrow = c(2, 2), las = 1)
plot(predict(ispl, nseg = 201),     # plots over the range of the knots
     main = "Original data with interpolating spline", type = "l",
     xlab = "height", ylab = "weight") 
points(women$height, women$weight, col = 4)
plot(predict(ispl, nseg = 201, deriv = 1),
     main = "First derivative of interpolating spline", type = "l",
     xlab = "height", ylab = "weight") 
plot(predict(ispl, nseg = 201, deriv = 2),
     main = "Second derivative of interpolating spline", type = "l",
     xlab = "height", ylab = "weight") 
plot(predict(ispl, nseg = 401, deriv = 3),
     main = "Third derivative of interpolating spline", type = "l",
     xlab = "height", ylab = "weight") 
par(opar)
}
\keyword{models}%-- one or more ...
