% File src/library/datasets/man/InsectSprays.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
% Distributed under GPL 2 or later

\name{InsectSprays}
\docType{data}
\alias{InsectSprays}
\title{Effectiveness of Insect Sprays}
\description{
  The counts of insects in agricultural experimental units treated with
  different insecticides.
}
\usage{InsectSprays}
\format{
  A data frame with 72 observations on 2 variables.
  \tabular{rlll}{
    [,1]  \tab \code{count}  \tab numeric  \tab Insect count\cr
    [,2]  \tab \code{spray}  \tab factor   \tab The type of spray
  }
}
\source{
  \bibcitet{R:Beall:1942}.
}
\references{
  \bibshow{*, R:McNeil:1977}
}
\examples{
require(stats); require(graphics)
boxplot(count ~ spray, data = InsectSprays,
        xlab = "Type of spray", ylab = "Insect count",
        main = "InsectSprays data", varwidth = TRUE, col = "lightgray")
fm1 <- aov(count ~ spray, data = InsectSprays)
summary(fm1)
opar <- par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0))
plot(fm1)
fm2 <- aov(sqrt(count) ~ spray, data = InsectSprays)
summary(fm2)
plot(fm2)
par(opar)
}
\keyword{datasets}
