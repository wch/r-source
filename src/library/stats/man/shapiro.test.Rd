% File src/library/stats/man/shapiro.test.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{shapiro.test}
\alias{shapiro.test}
\title{Shapiro-Wilk Normality Test}
\description{
  Performs the Shapiro-Wilk test of normality.
}
\usage{
shapiro.test(x)
}
\arguments{
  \item{x}{a numeric vector of data values. Missing values are allowed,
    but the number of non-missing values must be between 3 and 5000.}
}
\value{
  A list with class \code{"htest"} containing the following components:
  \item{statistic}{the value of the Shapiro-Wilk statistic.}
  \item{p.value}{an approximate p-value for the test.  This is
    said in Royston (1995) to be adequate for \code{p.value < 0.1}.}
  \item{method}{the character string \code{"Shapiro-Wilk normality test"}.}
  \item{data.name}{a character string giving the name(s) of the data.}
}
\references{
  Patrick Royston (1982)
  An extension of Shapiro and Wilk's \eqn{W} test for normality to large
  samples.
  \emph{Applied Statistics}, \bold{31}, 115--124.

  Patrick Royston (1982)
  Algorithm AS 181: The \eqn{W} test for Normality.
  \emph{Applied Statistics}, \bold{31}, 176--180.

  Patrick Royston (1995)
  Remark AS R94: A remark on Algorithm AS 181: The \eqn{W} test for normality.
  \emph{Applied Statistics}, \bold{44}, 547--551.
}
\source{
  The algorithm used is a C translation of the Fortran code described in
  Royston (1995) and found at \url{http://lib.stat.cmu.edu/apstat/R94}.
  The calculation of the p value is exact for \eqn{n = 3}, otherwise
  approximations are used, separately for \eqn{4 \le n \le 11} and
  \eqn{n \ge 12}.
}
\seealso{
  \code{\link{qqnorm}} for producing a normal quantile-quantile plot.
}
% FIXME: could use something more interesting here
\examples{
shapiro.test(rnorm(100, mean = 5, sd = 3))
shapiro.test(runif(100, min = 2, max = 4))
}
\keyword{htest}
