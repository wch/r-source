\name{predict.loess}
\alias{predict.loess}
\title{Predict Loess Curve or Surface}
\description{
 Predictions from a \code{loess} fit, optionally with standard errors.
}
\usage{
\method{predict}{loess}(object, newdata = NULL, se = FALSE, \dots)
}
\arguments{
 \item{object}{an object fitted by \code{loess}.}
 \item{newdata}{an optional data frame specifying points at which to do
   the predictions. If missing, the original data points are used.}
 \item{se}{should standard errors be computed?}
 \item{\dots}{arguments passed to or from other methods.}
}
\details{
  The standard errors calculation is slower than prediction.

  When the fit was made using \code{surface="interpolate"} (the
  default), \code{predict.loess} will not extrapolate -- so points outside
  an axis-aligned hypercube enclosing the original data will have
  missing (\code{NA}) predictions and standard errors.
}
\value{
  If \code{se = FALSE}, a vector giving the prediction for each row of
  \code{newdata} (or the original data). If \code{se = TRUE}, a list
  containing components
  \item{fit}{the predicted values.}
  \item{se}{an estimated standard error for each predicted value.}
  \item{residual.scale}{the estimated scale of the residuals used in
    computing the standard errors.}
  \item{df}{an estimate of the effective degrees of freedom used in
    estimating the residual scale, intended for use with t-based
    confidence intervals. }
}
\author{
  B.D. Ripley, based on the \code{cloess} package of Cleveland,
  Grosse and Shyu.
}

\seealso{\code{\link{loess}}}

\examples{
data(cars)
cars.lo <- loess(dist ~ speed, cars)
predict(cars.lo, data.frame(speed=seq(5, 30, 1)), se=TRUE)
# to get extrapolation
cars.lo2 <- loess(dist ~ speed, cars,
  control=loess.control(surface="direct"))
predict(cars.lo2, data.frame(speed=seq(5, 30, 1)), se=TRUE)
}
\keyword{smooth}
