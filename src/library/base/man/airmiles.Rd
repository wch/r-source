\name{airmiles}
\title{Commercial Airline Mileage}
\usage{data(airmiles)}
\alias{airmiles}
\format{A time-series of 24 observations; yearly, 1937-1960

}
\source{F.A.A. Statistical Handbook of Aviation.

}
\description{The revenue passenger miles flown by commercial airlines in
  the United States for each year from 1937 to 1960.
}
\references{Brown, R. G. (1963).  Smoothing, Forecasting and Prediction
  of Discrete Time Series.  Prentice-Hall.
}
\examples{
data(airmiles)
plot(airmiles, main = "airmiles data",
     xlab = "Passenger-miles flown by U.S. commercial airlines", col = 4)
}
\keyword{datasets}
