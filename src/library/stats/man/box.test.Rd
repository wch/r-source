% File src/library/stats/man/box.test.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{Box.test}
\alias{Box.test}
\concept{portmanteau}
\title{Box-Pierce and Ljung-Box Tests}
\description{
  Compute the Box--Pierce or Ljung--Box test statistic for examining the
  null hypothesis of independence in a given time series.  These are
  sometimes known as \sQuote{portmanteau} tests.
}
\usage{
Box.test(x, lag = 1, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)
}
\arguments{
  \item{x}{a numeric vector or univariate time series.}
  \item{lag}{the statistic will be based on \code{lag} autocorrelation
    coefficients.}
  \item{type}{test to be performed: partial matching is used.}
  \item{fitdf}{number of degrees of freedom to be subtracted if \code{x}
    is a series of residuals.}
}
\details{
  These tests are sometimes applied to the residuals from an
  \code{ARMA(p, q)} fit, in which case the references suggest a better
  approximation to the null-hypothesis distribution is obtained by
  setting \code{fitdf = p+q}, provided of course that \code{lag > fitdf}.
}
\note{
  Missing values are not handled.
}
\value{
  A list with class \code{"htest"} containing the following components:
  \item{statistic}{the value of the test statistic.}
  \item{parameter}{the degrees of freedom of the approximate chi-squared
    distribution of the test statistic (taking \code{fitdf} into account.}
  \item{p.value}{the p-value of the test.}
  \item{method}{a character string indicating which type of test was
    performed.}
  \item{data.name}{a character string giving the name of the data.}
}
\references{
  Box, G. E. P. and Pierce, D. A. (1970),
  Distribution of residual correlations in autoregressive-integrated
  moving average time series models.
  \emph{Journal of the American Statistical Association}, \bold{65},
  1509--1526.

  Ljung, G. M. and Box, G. E. P. (1978),
  On a measure of lack of fit in time series models.
  \emph{Biometrika} \bold{65}, 297--303.

  Harvey, A. C. (1993)
  \emph{Time Series Models}.
  2nd Edition, Harvester Wheatsheaf, NY, pp. 44, 45.
}
\author{A. Trapletti}
\examples{
x <- rnorm (100)
Box.test (x, lag = 1)
Box.test (x, lag = 1, type = "Ljung")
}
\keyword{ts}
