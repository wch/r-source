\name{longley}
\alias{longley}
\title{Longley's Regression Data}
\description{
  A macroeconomic data set which provides a well-known example for a
  highly collinear regression.
}
\usage{data(longley)}
\format{
  A data frame with 7 economical variables, observed yearly from 1947 to
  1962 (\eqn{n=16}).

  \tabular{ll}{
    GNP.deflator: \tab GNP implicit price deflator (\eqn{1954=100})\cr
    GNP:          \tab Gross National Producr.\cr
    Unemployed:   \tab number of unemployed \cr
    Armed.Forces: \tab number of .. in the armed forces\cr
    Population:   \tab `noninstitutionalized' population \eqn{\ge} 14
    years of age.\cr
    Year:         \tab the year (time).\cr
    Employed:     \tab number of people employed.
  }

  The regression \code{lm(Employed ~ .)} is known to be highly
  collinear.
}
\source{
  J. W. Longley (1967)
  An appraisal of least-squares programs from the point of view of the
  user.
  \emph{Journal of the American Statistical Association}, \bold{62},
  819--841.
}
\examples{
# give the data set in the form it is used in S-plus:
data(longley)
longley.x <- data.matrix(longley[, 1:6])
longley.y <- longley[, "Employed"]
pairs(longley, main = "longley data")
summary(fm1 <- lm(Employed ~ ., data = longley))
opar <- par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0),
            mar = c(4.1, 4.1, 2.1, 1.1))
plot(fm1)
par(opar)
}
\keyword{datasets}
