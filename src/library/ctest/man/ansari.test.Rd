\name{ansari.test}
\alias{ansari.test}
\title{Ansari-Bradley Test}
\description{
  Performs the Ansari-Bradley two-sample test for a difference in scale
  parameters.
}
\usage{
ansari.test(x, y, alternative = c("two.sided", "less", "greater"),
            exact = NULL)
}
\arguments{
  \item{x}{numeric vector of data values.}
  \item{y}{numeric vector of data values.}
  \item{alternative}{indicates the alternative hypothesis and must be
    one of \code{"two.sided"}, \code{"greater"} or \code{"less"}.  You
    can specify just the initial letter.}
  \item{exact}{a logical indicating whether an exact p-value should be
    computed.}
}
\details{
  Suppose that \code{x} and \code{y} are independent samples from
  distributions with densities \eqn{f((t-m)/s)/s} and \eqn{f(t-m)},
  respectively, where \eqn{m} is an unknown nuisance parameter and
  \eqn{s} is the parameter of interest.  The Ansari-Bradley test is used
  for testing the null that \eqn{s} equals 1, the two-sided alternative
  being that \eqn{s \ne 1}{s != 1} (the distributions differ only in
  variance), and the one-sided alternatives being \eqn{s > 1} (the
  distribution underlying \code{x} has a larger variance,
  \code{"greater"}) or \eqn{s < 1} (\code{"less"}).

  By default (if \code{exact} is not specified), an exact p-value is
  computed if both samples contain less than 50 finite values and there
  are no ties.  Otherwise, a normal approximation is used.
}
\value{
  A list with class \code{"htest"} containing the following components:
  \item{statistic}{the value of the Ansari-Bradley test statistic.}
  \item{p.value}{the p-value of the test.}
  \item{alternative}{a character string describing the alternative
    hypothesis.}
  \item{method}{the string \code{"Ansari-Bradley test"}.}
  \item{data.name}{a character string giving the names of the data.}
}
\references{
  Myles Hollander & Douglas A. Wolfe (1973),
  \emph{Nonparametric statistical inference}.
  New York: John Wiley & Sons.
  Pages 83--92.
}
\seealso{
  \code{\link{fligner.test}} for a rank-based (nonparametric) k-sample
  test for homogeneity of variances;
  \code{\link{mood.test}} for another rank-based two-sample test for a
  difference in scale parameters;
  \code{\link{var.test}} and \code{\link{bartlett.test}} for parametric
  tests for the homogeneity in variance.
}
\examples{
## Hollander & Wolfe (1973, p. 86f):
## Serum iron determination using Hyland control sera
ramsay <- c(111, 107, 100, 99, 102, 106, 109, 108, 104, 99,
            101, 96, 97, 102, 107, 113, 116, 113, 110, 98)
jung.parekh <- c(107, 108, 106, 98, 105, 103, 110, 105, 104,
            100, 96, 108, 103, 104, 114, 114, 113, 108, 106, 99)
ansari.test(ramsay, jung.parekh)
}
\keyword{htest}
