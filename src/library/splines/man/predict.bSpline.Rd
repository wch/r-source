\name{predict.bSpline}
\alias{predict.bSpline}
\alias{predict.nbSpline}
\alias{predict.pbSpline}
\alias{predict.npolySpline}
\alias{predict.ppolySpline}
%\alias{plot.spline}
\title{Evaluate a Spline at New Values of x}
\description{
  The \code{predict} methods for the classes that inherit from the
  virtual classes \code{bSpline} and \code{polySpline} are used to
  evaluate the spline or its derivatives.  The \code{plot} method for a
  spline object first evaluates \code{predict} with the \code{x}
  argument missing, then plots the resulting \code{xyVector} with
  \code{type = "l"}.}
\usage{
\method{predict}{bSpline}(object, x, nseg=50, deriv=0, \dots)
%plot.spline(x, \dots)
}
% \synopsis{
% predict.bSpline(object, x, nseg=50, deriv=0, \dots)
% predict.nbSpline(object, x, nseg=50, deriv=0, \dots)
% predict.pbSpline(object, x, nseg=50, deriv=0, \dots)
% predict.npolySpline(object, x, nseg=50, deriv=0, \dots)
% predict.ppolySpline(object, x, nseg=50, deriv=0, \dots)
% plot.spline(x, \dots)
% }
\arguments{
  \item{object}{An object that inherits from the \code{bSpline} or the
    \code{polySpline} class.  For \code{plot.spline} this argument is
    called \code{x}.}
  \item{x}{A numeric vector of \code{x} values at which to evaluate the
    spline.  If this argument is missing a suitable set of \code{x}
    values is generated as a sequence of \code{nseq} segments spanning
    the range of the knots.  For \code{plot.spline} the \code{x}
    argument is as described under \code{object} above.}
  \item{nseg}{A positive integer giving the number of segments in a set
    of equally-spaced \code{x} values spanning the range of the knots
    in \code{object}.  This value is only used if \code{x} is missing.}
  \item{deriv}{An integer between 0 and \code{splineOrder(object) - 1}
    specifying the derivative to evaluate.}
  \item{\dots}{
    \code{predict}: further arguments passed to or from other methods.

    \code{plot}: additional graphical parameters (see \code{link{par}}).
  }
}
\value{
  an \code{xyVector} with components
  \item{x}{the supplied or inferred numeric vector of \code{x} values}
  \item{y}{the value of the spline (or its \code{deriv}'th derivative)
      at the \code{x} vector}
}
\author{Douglas Bates and Bill Venables}
\seealso{
  \code{\link{xyVector}},
  \code{\link{interpSpline}},
  \code{\link{periodicSpline}}
}
\examples{
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
\keyword{models}
