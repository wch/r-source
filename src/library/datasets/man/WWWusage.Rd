% File src/library/datasets/man/WWWusage.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
% Distributed under GPL 2 or later

\name{WWWusage}
\docType{data}
\alias{WWWusage}
\title{Internet Usage per Minute}
\usage{WWWusage}
\description{
  A time series of the numbers of users connected to the Internet
  through a server every minute.
}
\format{
  A time series of length 100.
}
\source{
  \bibshow{R:Durbin+Koopman:2001}
}
\references{
  \bibshow{R:Makridakis+Wheelwright+Hyndman:1997}
}
\examples{
require(graphics)
work <- diff(WWWusage)
par(mfrow = c(2, 1)); plot(WWWusage); plot(work)
\dontrun{
require(stats)
aics <- matrix(, 6, 6, dimnames = list(p = 0:5, q = 0:5))
for(q in 1:5) aics[1, 1+q] <- arima(WWWusage, c(0, 1, q),
    optim.control = list(maxit = 500))$aic
for(p in 1:5)
   for(q in 0:5) aics[1+p, 1+q] <- arima(WWWusage, c(p, 1, q),
       optim.control = list(maxit = 500))$aic
round(aics - min(aics, na.rm = TRUE), 2)
}}
\keyword{datasets}
