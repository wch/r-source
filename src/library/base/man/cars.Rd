\name{cars}
\title{Stopping Distances of Cars}
\usage{data(cars)}
\alias{cars}
\format{A data frame with 50 observations on 2 variable.
  \tabular{rlll}{
    [,1]  \tab speed  \tab numeric  \tab Speed (mph)\cr
    [,2]  \tab dist   \tab numeric  \tab Stopping distance (ft)\cr
  }
}
\source{Ezekiel, M. (1930).  Methods of Correlation Analysis.  Wiley.
}
\description{The data give the speed of cars and the distances taken to stop.
Note that the data were recorded in the 1920s.
}
\references{McNeil, D. R. (1977).  Interactive Data Analysis.  Wiley.
}
\examples{
data(cars)
plot(cars, xlab = "Speed (mph)", ylab = "Stopping distance (ft)",
     las = 1)
lines(lowess(cars$speed, cars$dist, f = 2/3, iter = 3), col = "red")
title(main = "cars data")
plot(cars, xlab = "Speed (mph)", ylab = "Stopping distance (ft)",
     las = 1, log = "xy")
title(main = "cars data (logarithmic scales)")
lines(lowess(cars$speed, cars$dist, f = 2/3, iter = 3), col = "red")
summary(fm1 <- lm(log(dist) ~ log(speed), data = cars))
opar <- par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0),
            mar = c(4.1, 4.1, 2.1, 1.1))
plot(fm1)
par(opar)
\keyword{datasets}
