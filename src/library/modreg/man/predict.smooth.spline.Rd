% file modreg/man/predict.smooth.spline.Rd
% copyright (C) 1998 B. D. Ripley
%
\name{predict.smooth.spline}
\title{Predict from Smoothing Spline Fit}
\usage{
predict.smooth.spline(object, x, deriv=0)
}
\alias{predict.smooth.spline}
\arguments{
 \item{object}{a fit from \code{smooth.spline}.}
 \item{x}{the new values of x.}
 \item{deriv}{the order of the derivative required.}
}
\description{
 Predict a smoothing spline fit at new points, return the derivative if
 desired. The predicted fit is linear beyond the original data.
}
\value{
  A list with components
  \item{x}{The input x.}
  \item{y}{The fitted values or derivatives at \code{x}.}
}
\author{B.D. Ripley}

\seealso{\code{\link{smooth.spline}}}

\examples{
data(cars)
attach(cars)
plot(speed, dist)
cars.spl <- smooth.spline(speed, dist, df=5)
lines(predict(cars.spl,seq(0,25, 0.5)))
detach()
}
\keyword{smooth}
