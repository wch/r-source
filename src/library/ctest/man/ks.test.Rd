\name{ks.test}
\alias{ks.test}
\title{Kolmogorov-Smirnov Tests}
\description{
  Performs one or two sample Kolmogorov-Smirnov tests.
}
\usage{
ks.test(x, y, \dots, alternative = c("two.sided", "less", "greater"),
        exact = NULL)
}
\arguments{
  \item{x}{a numeric vector of data values.}
  \item{y}{either a numeric vector of data values, or a character string
    naming a distribution function.}
  \item{\dots}{parameters of the distribution specified by \code{y}.}
  \item{alternative}{indicates the alternative hypothesis and must be
    one of \code{"two.sided"} (default), \code{"less"}, or
    \code{"greater"}.  You can specify just the initial letter.}
  \item{exact}{a logical indicating whether an exact p-value should be
    computed.  Only used in the two-sided two-sample case.}
  
}
\details{
  If \code{y} is numeric, a two sample test of the null that \code{x}
  and \code{y} were drawn from the same distribution is performed.

  Alternatively, \code{y} can be a character string naming a
  distribution function.  In this case, a one sample test of the null
  that the distribution function underlying \code{x} is \code{y} with
  parameters specified by \code{\dots} is carried out.

  The possible values \code{"two.sided"}, \code{"less"} and
  \code{"greater"} of \code{alternative} specify the null hypothesis
  that the true distribution function of \code{x} is equal to, not less
  than or not greater than the hypothesized distribution function
  (one-sample case) or the distribution function of \code{y} (two-sample
  case), respectively.

  Currently, exact p-value are only available for the two-sided
  two-sample test.  In this case, by default (if \code{exact} is not
  specified), an exact p-value is computed if the product of the sample
  sizes is less than 10000.  Otherwise, the asymptotic distributions are
  used.  This approximation may be inaccurate in small samples.
}
\value{
  A list with class \code{"htest"} containing the following components:
  \item{statistic}{the value of the test statistic.}
  \item{p.value}{the p-value of the test.}
  \item{alternative}{a character string describing the alternative
    hypothesis.}
  \item{method}{a character string indicating what type of test was
    performed.}
  \item{data.name}{a character string giving the name(s) of the data.}
}
\references{
  Conover, W. J. (1971),
  \emph{Practical nonparametric statistics}.
  New York: John Wiley & Sons.
  Pages 295--301 (one-sample ``Kolmogorov'' test),
  309--314 (two-sample ``Smirnov'' test).
}
\seealso{
  \code{\link{shapiro.test}} which performs the Shapiro-Wilk test for
  normality.
}
\examples{
x <- rnorm(50)
y <- runif(30)
# Do x and y come from the same distribution?
ks.test(x, y)
# Does x come from a shifted gamma distribution with shape 3 and scale 2?
ks.test(x+2, "pgamma", 3, 2) # two-sided
ks.test(x+2, "pgamma", 3, 2, alternative = "gr")
}
\keyword{htest}
