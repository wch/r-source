\name{Nile}
\alias{Nile}
\non_function{}
\title{Flow of the River Nile}
\usage{data(Nile)}
\description{
  Measurements of the annual flow of the river Nile at Ashwan 1871--1970.
}
\format{
  A time series of length 100.
}
\source{
  Durbin, J. and Koopman, S. J. (2001) \emph{Time Series Analysis by
    State Space Methods.}  Oxford University Press.
  \url{http://www.ssfpack.com/dkbook/}
}
\references{
  Balke, N. S. (1993) Detecting level shifts in time series.
  \emph{Journal of Business and Economic Statistics} \bold{11}, 81--92.
  
  Cobb, G. W. (1978) The problem of the Nile: conditional solution to a
  change-point problem.  \emph{Biometrika} \bold{65}, 243--51.
}
\examples{
data(Nile)
par(mfrow = c(2,2))
plot(Nile)
acf(Nile)
pacf(Nile)
ar(Nile) # selects order 2
cpgram(ar(Nile)$resid)
par(mfrow = c(1,1))
arima(Nile, c(2, 0, 0))

## Now consider missing values, following Durbin & Koopman
NileNA <- Nile
NileNA[c(21:40, 61:80)] <- NA
arima(NileNA, c(2, 0, 0))
plot(NileNA)
pred <- predict(arima(window(NileNA, 1871, 1890), c(2, 0, 0)), n.ahead = 20)
lines(pred$pred, lty = 3, col = "red")
lines(pred$pred + 2*pred$se, lty=2, col="blue")
lines(pred$pred - 2*pred$se, lty=2, col="blue")
pred <- predict(arima(window(NileNA, 1871, 1930), c(2, 0, 0)), n.ahead = 20)
lines(pred$pred, lty = 3, col = "red")
lines(pred$pred + 2*pred$se, lty=2, col="blue")
lines(pred$pred - 2*pred$se, lty=2, col="blue")
}
\keyword{datasets}
