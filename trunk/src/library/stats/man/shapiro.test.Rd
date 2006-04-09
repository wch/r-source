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
  \item{x}{a numeric vector of data values, the number of which must be
    between 3 and 5000.  Missing values are allowed.}
}
\value{
  A list with class \code{"htest"} containing the following components:
  \item{statistic}{the value of the Shapiro-Wilk statistic.}
  \item{p.value}{the p-value for the test.}
  \item{method}{the character string \code{"Shapiro-Wilk normality test"}.}
  \item{data.name}{a character string giving the name(s) of the data.}
}
\references{
  Patrick Royston (1982)
  An Extension of Shapiro and Wilk's \eqn{W} Test for Normality to Large
  Samples.
  \emph{Applied Statistics}, \bold{31}, 115--124.

  Patrick Royston (1982)
  Algorithm AS 181: The \eqn{W} Test for Normality.
  \emph{Applied Statistics}, \bold{31}, 176--180.

  Patrick Royston (1995)
  A Remark on Algorithm AS 181: The \eqn{W} Test for Normality.
  \emph{Applied Statistics}, \bold{44}, 547--551.
}
\seealso{
  \code{\link[base:qqnorm]{qqnorm}} for producing a normal quantile-quantile plot.
}
\examples{
% FIXME: could use something more interesting here
shapiro.test(rnorm(100, mean = 5, sd = 3))
shapiro.test(runif(100, min = 2, max = 4))
}
\keyword{htest}
