% File src/library/datasets/man/UKDriverDeaths.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\newcommand{\sspace}{\ifelse{latex}{\out{~}}{ }}

\name{UKDriverDeaths}
\docType{data}
\alias{UKDriverDeaths}
\alias{Seatbelts}
\title{
  Road Casualties in Great Britain 1969--84
}
\description{
  \code{UKDriverDeaths} is a time series giving the monthly totals
  of car drivers in
  Great Britain killed or seriously injured Jan 1969 to Dec 1984.
  Compulsory wearing of seat belts was introduced on 31 Jan 1983.

  \code{Seatbelts} is more information on the same problem.
}
\usage{
UKDriverDeaths
Seatbelts
}
\format{
  \code{Seatbelts} is a multiple time series, with columns
  \describe{
    \item{\code{DriversKilled}}{car drivers killed.}
    \item{\code{drivers}}{same as \code{UKDriverDeaths}.}
    \item{\code{front}}{front-seat passengers killed or seriously injured.}
    \item{\code{rear}}{rear-seat passengers killed or seriously injured.}
    \item{\code{kms}}{distance driven.}
    \item{\code{PetrolPrice}}{petrol price.}
    \item{\code{VanKilled}}{number of van (\sQuote{light goods vehicle})
      drivers.}
    \item{\code{law}}{0/1: was the law in effect that month?}
  }
}
\source{
  Harvey, A.C. (1989)
  \emph{Forecasting, Structural Time Series Models and the Kalman Filter.}
  Cambridge University Press, pp.\sspace{}519--523.

  Durbin, J. and Koopman, S. J. (2001) \emph{Time Series Analysis by
    State Space Methods.}  Oxford University Press.
  \url{http://www.ssfpack.com/dkbook/}
}
\references{
  Harvey, A. C. and Durbin, J. (1986) The effects of seat belt
  legislation on British road casualties: A case study in structural
  time series modelling. \emph{Journal of the Royal Statistical Society}
  series B, \bold{149}, 187--227.
}
\examples{
require(stats); require(graphics)
## work with pre-seatbelt period to identify a model, use logs
work <- window(log10(UKDriverDeaths), end = 1982+11/12)
par(mfrow = c(3, 1))
plot(work); acf(work); pacf(work)
par(mfrow = c(1, 1))
(fit <- arima(work, c(1, 0, 0), seasonal = list(order = c(1, 0, 0))))
z <- predict(fit, n.ahead = 24)
ts.plot(log10(UKDriverDeaths), z$pred, z$pred+2*z$se, z$pred-2*z$se,
        lty = c(1, 3, 2, 2), col = c("black", "red", "blue", "blue"))

## now see the effect of the explanatory variables
X <- Seatbelts[, c("kms", "PetrolPrice", "law")]
X[, 1] <- log10(X[, 1]) - 4
arima(log10(Seatbelts[, "drivers"]), c(1, 0, 0),
      seasonal = list(order = c(1, 0, 0)), xreg = X)
}
\keyword{datasets}
