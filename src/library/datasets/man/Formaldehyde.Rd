\name{Formaldehyde}
\docType{data}
\alias{Formaldehyde}
\title{Determination of Formaldehyde}
\description{
  These data are from a chemical experiment to prepare a standard curve
  for the determination of formaldehyde by the addition of chromatropic
  acid and concentrated sulpuric acid and the reading of the resulting
  purple color on a spectrophotometer.
}
\usage{Formaldehyde}
\format{
  A data frame with 6 observations on 2 variables.
  \tabular{rlll}{
    [,1] \tab carb\tab numeric \tab Carbohydrate (ml) \cr
    [,2] \tab optden \tab numeric \tab Optical Density
  }
}
\source{
  Bennett, N. A. and N. L. Franklin (1954)
  \emph{Statistical Analysis in Chemistry and the Chemical Industry}.
  New York: Wiley.
}
\references{
  McNeil, D. R. (1977) \emph{Interactive Data Analysis.}
  New York: Wiley.
}
\examples{
require(stats); require(graphics)
plot(optden ~ carb, data = Formaldehyde,
     xlab = "Carbohydrate (ml)", ylab = "Optical Density",
     main = "Formaldehyde data", col = 4, las = 1)
abline(fm1 <- lm(optden ~ carb, data = Formaldehyde))
summary(fm1)
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(fm1)
par(opar)
}
\keyword{datasets}
