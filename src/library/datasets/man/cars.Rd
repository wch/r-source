% File src/library/datasets/man/cars.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{cars}
\docType{data}
\alias{cars}
\title{Speed and Stopping Distances of Cars}
\description{
  The data give the speed of cars and the distances taken to stop.
  Note that the data were recorded in the 1920s.
}
\usage{cars}
\format{
  A data frame with 50 observations on 2 variables.
  \tabular{rlll}{
    [,1]  \tab speed  \tab numeric  \tab Speed (mph)\cr
    [,2]  \tab dist   \tab numeric  \tab Stopping distance (ft)
  }
}
\source{
  Ezekiel, M. (1930)
  \emph{Methods of Correlation Analysis}.
  Wiley.
}
\references{
  McNeil, D. R. (1977)
  \emph{Interactive Data Analysis}.
  Wiley.
}
\examples{
require(stats); require(graphics)
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

## An example of polynomial regression
plot(cars, xlab = "Speed (mph)", ylab = "Stopping distance (ft)",
    las = 1, xlim = c(0, 25))
d <- seq(0, 25, length.out = 200)
for(degree in 1:4) {
  fm <- lm(dist ~ poly(speed, degree), data = cars)
  assign(paste("cars", degree, sep = "."), fm)
  lines(d, predict(fm, data.frame(speed = d)), col = degree)
}
anova(cars.1, cars.2, cars.3, cars.4)
}
\keyword{datasets}
