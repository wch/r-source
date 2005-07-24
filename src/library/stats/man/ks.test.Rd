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
  \item{\dots}{parameters of the distribution specified (as a character
    string) by \code{y}.}
  \item{alternative}{indicates the alternative hypothesis and must be
    one of \code{"two.sided"} (default), \code{"less"}, or
    \code{"greater"}.  You can specify just the initial letter.}
  \item{exact}{\code{NULL} or a logical indicating whether an exact
    p-value should be computed.  See Details for the meaning of
    \code{NULL}.  Only used in the two-sided cases.}
  
}
\details{
  If \code{y} is numeric, a two-sample test of the null hypothesis
  that \code{x} and \code{y} were drawn from the same \emph{continuous}
  distribution is performed.

  Alternatively, \code{y} can be a character string naming a continuous
  distribution function.  In this case, a one-sample test is carried
  out of the null that the distribution function which generated
  \code{x} is distribution \code{y} with parameters specified by
  \code{\dots}.

  The presence of ties generates a warning, since continuous
  distributions do not generate them.

  The possible values \code{"two.sided"}, \code{"less"} and
  \code{"greater"} of \code{alternative} specify the null hypothesis
  that the true distribution function of \code{x} is equal to, not less
  than or not greater than the hypothesized distribution function
  (one-sample case) or the distribution function of \code{y} (two-sample
  case), respectively.

  Exact p-values are available for the two-sided tests with no ties.  In
  that case.  If \code{exact = NULL} (the default), an exact p-value is
  computed if the sample size if less than 100 in the one-sample case,
  and if the product of the sample sizes is less than 10000 in the
  two-sample case.  Otherwise, asymptotic distributions are used whose
  approximations may be inaccurate in small samples.  In the one-sample
  two-sided case, exact p-values are obtained as described in Marsaglia,
  Tsang & Wang (2003).

  If a single-sample test is used, the parameters specified in
  \code{\dots} must be pre-specified and not estimated from the data.
  There is some more refined distribution theory for the KS test with
  estimated parameters (see Durbin, 1973), but that is not implemented
  in \code{ks.test}.
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
  William J. Conover (1971),
  \emph{Practical nonparametric statistics}.
  New York: John Wiley & Sons.
  Pages 295--301 (one-sample \dQuote{Kolmogorov} test),
  309--314 (two-sample \dQuote{Smirnov} test).

  Durbin, J. (1973)
  \emph{Distribution theory for tests based on the sample distribution
    function}.  SIAM.

  George Marsaglia, Wai Wan Tsang & Jingbo Wang (2003),
  Evaluating Kolmogorov's distribution.
  \emph{Journal of Statistical Software}, \bold{8}/18.
  \url{http://www.jstatsoft.org/v08/i18/}.
  
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
ks.test(x+2, "pgamma", 3, 2) # two-sided, exact
ks.test(x+2, "pgamma", 3, 2, exact = FALSE)
ks.test(x+2, "pgamma", 3, 2, alternative = "gr")
}
\keyword{htest}
