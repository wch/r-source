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
    [,1]  \tab count  \tab numeric  \tab Insect count\cr
    [,2]  \tab spray  \tab factor   \tab The type of spray
  }
}
\source{
  Beall, G., (1942)
  The Transformation of data from entomological field experiments,
  \emph{Biometrika}, \bold{29}, 243--262.
}
\references{
  McNeil, D. (1977) \emph{Interactive Data Analysis}.
  New York: Wiley.
}
\examples{
require(stats); require(graphics)
boxplot(count ~ spray, data = InsectSprays,
        xlab = "Type of spray", ylab = "Insect count",
        main = "InsectSprays data", varwidth = TRUE, col = "lightgray")
fm1 <- aov(count ~ spray, data = InsectSprays)
summary(fm1)
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(fm1)
fm2 <- aov(sqrt(count) ~ spray, data = InsectSprays)
summary(fm2)
plot(fm2)
par(opar)
}
\keyword{datasets}
