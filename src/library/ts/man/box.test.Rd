\name{box.test}
\alias{Box.test}
\title{Box-Pierce and Ljung-Box Tests}
\usage{
Box.test (x, lag = 1, type=c("Box-Pierce", "Ljung-Box"))
}
\arguments{
  \item{x}{a numeric vector or univariate time series.}
  \item{lag}{the statistic will be based on \code{lag}
      autocorrelation coefficients.}
  \item{type}{test to be performed: partial matching is used.}
}
\description{
  Compute the Box-Pierce or Ljung-Box test statistic
  for examining the null hypothesis of independence in the
  time series \code{x} is computed.
}
\note{
  Missing values are not handled.
}
\value{
  A list with class \code{"htest"} containing the following components:
  \item{statistic}{the value of the test statistic.}
  \item{parameter}{the degrees of freedom of the approximate chi-square
    distribution of the test statistic.}
  \item{p.value}{the p-value of the test.}
  \item{method}{a character string indicating which type of test was
    performed.}
  \item{data.name}{a character string giving the name of the data.}
}
\references{
  A. C. Harvey (1993) \emph{Time Series Models},
  2nd Edition, Harvester Wheatsheaf, NY, pp. 44, 45.
}
\author{A. Trapletti}
\examples{
x <- rnorm (100)
Box.test (x, lag = 1)
Box.test (x, lag = 1, type="Ljung")
}
\keyword{ts}

