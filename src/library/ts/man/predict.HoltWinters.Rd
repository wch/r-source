\name{predict.HoltWinters}
\alias{predict.HoltWinters}
\title{prediction function for fitted Holt-Winters models}
\description{
  Computes predictions and prediction intervals for models fitted by
  the Holt-Winters method.
}
\usage{
\method{predict}{HoltWinters}(object, n.ahead=1, prediction.interval = FALSE,
                    quantile = qnorm(0.975), \dots)
}
\arguments{
  \item{object}{An object of class \code{HoltWinters}}
  \item{n.ahead}{Number of future periods to predict}
  \item{prediction.interval}{logical. If \code{TRUE}, the lower and
    upper bounds of the corresponding prediction intervals are computed}
  \item{quantile}{Quantile used for each one-sided prediction interval
    (default: the 0.975 quantile to give a two-sided 0.95 interval)}
  \item{\dots}{arguments passed to or from other methods.}
}
\value{
  A time series of the predicted values. If prediction intervals are
  requested, a multiple time series is returned with columns \code{fit},
  \code{lwr} and \code{upr} for the predicted values and the lower and
  upper bounds respectively.
}
\references{
  C.C Holt (1957)
  Forecasting seasonals and trends by exponentially weighted
  moving averages,
  ONR Research Memorandum, Carnigie Institute 52.
  
  P.R Winters (1960)
  Forecasting sales by exponentially weighted moving averages,
  \emph{Management Science} \bold{6}, 324--342.
}
\author{
  David Meyer \email{david.meyer@ci.tuwien.ac.at}
}
\seealso{\code{\link{HoltWinters}}}

\examples{
data(co2)
m <- HoltWinters(co2)
p <- predict(m, 50, prediction.interval = TRUE)
plot(m, p)
}
\keyword{ts}


