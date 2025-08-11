% File src/library/stats/man/shapiro.test.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
% Distributed under GPL 2 or later

\name{shapiro.test}
\alias{shapiro.test}
\title{\I{Shapiro}-\I{Wilk} Normality Test}
\description{
  Performs the \I{Shapiro}-\I{Wilk} test of normality.
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
  \item{statistic}{the value of the \I{Shapiro}-\I{Wilk} statistic.}
  \item{p.value}{an approximate p-value for the test.  This is
    said in \bibcitet{R:Royston:1995} to be adequate for \code{p.value < 0.1}.}
  \item{method}{the character string \code{"Shapiro-Wilk normality test"}.}
  \item{data.name}{a character string giving the name(s) of the data.}
}
\source{
  The algorithm used is a C translation of the Fortran code described in
  \bibcitet{R:Royston:1995}. % and was found at \url{http://lib.stat.cmu.edu/apstat/R94}.
  The calculation of the p value is exact for \eqn{n = 3}, otherwise
  approximations are used, separately for \eqn{4 \le n \le 11} and
  \eqn{n \ge 12}.
}
\references{
  \bibshow{*, R:Royston:1982a, R:Royston:1982b}
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
