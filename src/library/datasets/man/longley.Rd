\name{longley}
\docType{data}
\alias{longley}
\title{Longley's Economic Regression Data}
\description{
  A macroeconomic data set which provides a well-known example for a
  highly collinear regression.
}
\usage{longley}
\format{
  A data frame with 7 economical variables, observed yearly from 1947 to
  1962 (\eqn{n=16}).
  \describe{
    \item{GNP.deflator:}{GNP implicit price deflator (\eqn{1954=100})}
    \item{GNP:}{Gross National Product.}
    \item{Unemployed:}{number of unemployed.}
    \item{Armed.Forces:}{number of people in the armed forces.}
    \item{Population:}{\sQuote{noninstitutionalized} population
      \eqn{\ge} 14 years of age.}
    \item{Year:}{the year (time).}
    \item{Employed:}{number of people employed.}
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
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\examples{
require(stats); require(graphics)
## give the data set in the form it is used in S-PLUS:
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
