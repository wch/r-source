\name{mood.test}
\alias{mood.test}
\title{Mood Two-Sample Test of Scale}
\description{
  Performs Mood's two-sample test for a difference in scale parameters.
}
\usage{
mood.test(x, y, alternative = c("two.sided", "less", "greater"))
}
\arguments{
  \item{x, y}{numeric vectors of data values.}
  \item{alternative}{indicates the alternative hypothesis and must be
    one of \code{"two.sided"} (default), \code{"greater"} or
    \code{"less"} all of which can be abbreviated.}
}
\details{
  The underlying model is that the two samples are drawn from
  \eqn{f(x-l)} and \eqn{f((x-l)/s)/s}, respectively, where \eqn{l} is a
  common location parameter and \eqn{s} is a scale parameter.

  The null hypothesis is \eqn{s = 1}.

  There are more useful tests for this problem.
}
\value{
  A list with class \code{"htest"} containing the following components:
  \item{statistic}{the value of the test statistic.}
  \item{p.value}{the p-value of the test.}
  \item{alternative}{a character string describing the alternative
    hypothesis.}
  \item{method}{the string \code{"Mood two-sample test of scale"}.}
  \item{data.name}{a character string giving the names of the data.}
}
\references{
  Conover, W. J. (1971),
  \emph{Practical nonparametric statistics}.
  New York: John Wiley & Sons.
  Pages 234f.
}
\seealso{
  \code{\link{fligner.test}} for a rank-based (nonparametric) k-sample
  test for homogeneity of variances;
  \code{\link{ansari.test}} for another rank-based two-sample test for a
  difference in scale parameters;
  \code{\link{var.test}} and \code{\link{bartlett.test}} for parametric
  tests for the homogeneity in variance.
}
\examples{
## Same data as for the Ansari-Bradley test:
## Serum iron determination using Hyland control sera
ramsay <- c(111, 107, 100, 99, 102, 106, 109, 108, 104, 99,
            101, 96, 97, 102, 107, 113, 116, 113, 110, 98)
jung.parekh <- c(107, 108, 106, 98, 105, 103, 110, 105, 104,
            100, 96, 108, 103, 104, 114, 114, 113, 108, 106, 99)
mood.test(ramsay, jung.parekh)
## Compare this to ansari.test(ramsay, jung.parekh)
}
\keyword{htest}
