\name{freeny}
\title{Freeny's Revenue Data}
\usage{data(freeny)}
\alias{freeny}
\format{
  \tabular{ll}{
    y: \tab quarterly revenue, 39 observations from (1962,2Q) to (1971,4Q).\cr
    x: \tab matrix of explanatory variables.  Columns are y lagged 1 \cr
       \tab quarter, price index, income level, and market potential.
     }
}
\source{A. E. Freeny (1977). A Portable Linear Regression Package with Test Programs.
Bell Laboratories memorandum.
}
\examples{
data(freeny)
summary(freeny)
pairs(freeny, main = "freeny data")
summary(fm1 <- lm(y ~ ., data = freeny))
opar <- par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0),
            mar = c(4.1, 4.1, 2.1, 1.1))
plot(fm1)
par(opar)
\keyword{datasets}
