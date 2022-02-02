% File src/library/datasets/man/nottem.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{nottem}
\docType{data}
\alias{nottem}
\title{
  Average Monthly Temperatures at Nottingham, 1920--1939
}
\description{
  A time series object containing average air temperatures at
  Nottingham Castle in degrees Fahrenheit for 20 years.
}
\usage{nottem}
\source{
  Anderson, O. D. (1976)
  \emph{Time Series Analysis and Forecasting: The Box-Jenkins approach.}
  Butterworths. Series R.
}
\examples{
\donttest{require(stats); require(graphics)
nott <- window(nottem, end = c(1936,12))
fit <- arima(nott, order = c(1,0,0), list(order = c(2,1,0), period = 12))
nott.fore <- predict(fit, n.ahead = 36)
ts.plot(nott, nott.fore$pred, nott.fore$pred+2*nott.fore$se,
        nott.fore$pred-2*nott.fore$se, gpars = list(col = c(1,1,4,4)))
}}
\keyword{datasets}
