% File src/library/stats/man/predict.smooth.spline.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{predict.smooth.spline}
\alias{predict.smooth.spline}
\title{Predict from Smoothing Spline Fit}
\description{
  Predict a smoothing spline fit at new points, return the derivative if
  desired. The predicted fit is linear beyond the original data.
}
\usage{
\method{predict}{smooth.spline}(object, x, deriv = 0, \dots)
}
\arguments{
  \item{object}{a fit from \code{smooth.spline}.}
  \item{x}{the new values of x.}
  \item{deriv}{integer; the order of the derivative required.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\value{
  A list with components
  \item{x}{The input \code{x}.}
  \item{y}{The fitted values or derivatives at \code{x}.}
}
\seealso{\code{\link{smooth.spline}}
}
\examples{
require(graphics)

attach(cars)
cars.spl <- smooth.spline(speed, dist, df = 6.4)
\dontshow{
print.default(cars.spl)
}

## "Proof" that the derivatives are okay, by comparing with approximation
diff.quot <- function(x, y) {
  ## Difference quotient (central differences where available)
  n <- length(x); i1 <- 1:2; i2 <- (n-1):n
  c(diff(y[i1]) / diff(x[i1]), (y[-i1] - y[-i2]) / (x[-i1] - x[-i2]),
    diff(y[i2]) / diff(x[i2]))
}

xx <- unique(sort(c(seq(0, 30, by = .2), kn <- unique(speed))))
i.kn <- match(kn, xx)   # indices of knots within xx
op <- par(mfrow = c(2,2))
plot(speed, dist, xlim = range(xx), main = "Smooth.spline & derivatives")
lines(pp <- predict(cars.spl, xx), col = "red")
points(kn, pp$y[i.kn], pch = 3, col = "dark red")
mtext("s(x)", col = "red")
for(d in 1:3){
  n <- length(pp$x)
  plot(pp$x, diff.quot(pp$x,pp$y), type = "l", xlab = "x", ylab = "",
       col = "blue", col.main = "red",
       main = paste0("s" ,paste(rep("'", d), collapse = ""), "(x)"))
  mtext("Difference quotient approx.(last)", col = "blue")
  lines(pp <- predict(cars.spl, xx, deriv = d), col = "red")
\dontshow{
  print(pp)
}
  points(kn, pp$y[i.kn], pch = 3, col = "dark red")
  abline(h = 0, lty = 3, col = "gray")
}
detach(); par(op)
}
\keyword{smooth}
