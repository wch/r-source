\name{chisq.test}
\title{Pearson's Chi-square Test for Count Data}
\usage{
chisq.test(x, y = NULL, correct = TRUE,
           p = rep(1/length(x), length(x)))
}
\alias{chisq.test}
\arguments{
  \item{x}{a vector or matrix.}
  \item{y}{a vector; ignored if \code{x} is a matrix.}
  \item{correct}{a logical indicating whether to apply continuity
    correction when computing the test statistic.}
}
\description{
  \code{chisq.test} performs chi-square tests on contingency tables.

  If \code{x} is a matrix with one row or column, or if \code{x} is a
  vector and \code{y} is not given, \code{x} is treated as a
  one-dimensional contingency table.  In this case, the hypothesis
  tested is whether the population probabilities equal those in
  \code{p}, or are all equal if \code{p} is not given.

  If \code{x} is a matrix with at least two rows and columns, it is
  taken as a two-dimensional contingency table, and hence its entries
  should be nonnegative integers.  Otherwise, \code{x} and \code{y} must
  be vectors or factors of the same length;  incomplete cases are
  removed, the objects are coerced into factor objects, and the
  contingency table is computed from these.  Then, Pearson's chi-square
  test of the null that the joint distribution of the cell counts in a
  2-dimensional contigency table is the product of the row and column
  marginals is performed.  Continuity correction is only used in the
  2-by-2 case if \code{correct} is \code{TRUE}.
}
\value{
  A list with class \code{"htest"} containing the following components:
  \item{statistic}{the value the chi-square test statistic.}
  \item{parameter}{the degrees of freedom of the approximate chi-square
    distribution of the test statistic.}
  \item{p.value}{the p-value for the test.}
  \item{method}{a character string indicating the type of test
    performed, and whether continuity correction was used.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{observed}{the observed counts.}
  \item{expected}{the expected counts under the null hypothesis.}
}
\examples{
data(insects)                   # Not really a good example
chisq.test(insects$count > 7, insects$spray)
                                # Prints test summary
chisq.test(insects$count > 7, insects$spray)$obs
                                # Counts observed
chisq.test(insects$count > 7, insects$spray)$obs
                                # Counts expected under the null
}
\keyword{htest}
\keyword{distribution}
