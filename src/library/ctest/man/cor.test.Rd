\name{cor.test}
\alias{cor.test}
\title{Test for Zero Correlation}
\description{
  Tests whether two samples come from uncorrelated (independent)
  populations, using Pearson's product moment correlation coefficient,
  Kendall's tau, or Spearman's rho.
}
\usage{
cor.test(x, y,
         alternative = c("two.sided", "less", "greater"),
         method = c("pearson", "kendall", "spearman"), exact = NULL)
}
\arguments{
  \item{x, y}{numeric vectors of data values.  \code{x} and \code{y}
    must have the same length.}
  \item{alternative}{indicates the alternative hypothesis and must be
    one of \code{"two.sided"}, \code{"greater"} or \code{"less"}.  You
    can specify just the initial letter.}
  \item{method}{a string indicating which correlation coefficient is
    used for the test.  One of \code{"pearson"},
    \code{"kendall"}, or \code{"spearman"}, can be abbreviated.}
  \item{exact}{a logical indicating whether an exact p-value should be
    computed.}
}
\value{
  A list with class \code{"htest"} containing the following components:
  \item{statistic}{the value of the test statistic.}
  \item{parameter}{the degrees of freedom of the test statistic in the
    case that it follows a t distribution.}
  \item{p.value}{the p-value of the test.}
  \item{estimate}{the estimated correlation coefficient, with names
    attribute \code{"cor"}, \code{"tau"}, or \code{"rho"}, correspoding
    to the method employed.}
  \item{null.value}{the value of the correlation coefficient under the
    null hypothesis, hence \code{0}.}
  \item{alternative}{a character string describing the alternative
    hypothesis.}
  \item{method}{a string indicating how the correlation was estimated}
  \item{data.name}{a character string giving the names of the data.}
}
\details{
  If \code{method} is \code{"pearson"}, the test statistic is based on
  Pearson's product moment correlation coefficient \code{cor(x, y)} and
  follows a t distribution with \code{length(x)-2} degrees of freedom.

  If \code{method} is \code{"kendall"} or \code{"spearman"}, Kendall's
  tau or Spearman's rho, respectively, are used to estimate the
  correlation.  These tests should be used if the data do not
  necessarily come from a bivariate normal distribution.

  For Kendall's test, by default (if \code{exact} is not specified), an
  exact p-value is computed if both samples contain less than 50 finite
  values and there are no ties.  Otherwise, the standardized estimate is
  used as the test statistic, and is approximately normally distributed.

  For Spearman's test, p-values are computed using algorithm AS 89.
}
\references{
  D. J. Best & D. E. Roberts (1975),
  Algorithm AS 89: The Upper Tail Probabilities of Spearman's
  \eqn{\rho}{rho}.
  \emph{Applied Statistics}, \bold{24}, 377--379.

  Myles Hollander & Douglas A. Wolfe (1973),
  \emph{Nonparametric statistical inference}.
  New York: John Wiley & Sons.
  Pages 185--194 (Kendall and Spearman tests).
}
\examples{
## Hollander & Wolfe (1973), p. 187f.
## Assessment of tuna quality.  We compare the Hunter L measure of
##  lightness to the averages of consumer panel scores (recoded as
##  integer values from 1 to 6 and averaged over 80 such values) in
#   9 lots of canned tuna.
##  The null is that the Hunter L value is positively associated
##  with the panel score.
x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)
cor.test(x, y, method = "kendall", alternative = "greater")
## => p=0.05972
##
cor.test(x, y, method = "kendall", alternative = "greater",
         exact = FALSE) # using large sample approximation
## => p=0.04765
## Compare this to
cor.test(x, y, method = "spearm", alternative = "g")
cor.test(x, y,                    alternative = "g")
}
\keyword{htest}
