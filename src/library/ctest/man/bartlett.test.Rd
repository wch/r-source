\name{bartlett.test}
\alias{bartlett.test}
\title{Bartlett Test for Homogeneity of Variances}
\description{
  Performs Bartlett's test of the null that the variances in each of the
  groups (samples) are the same.
}
\usage{
bartlett.test(x, g)
}
\arguments{
  \item{x}{a numeric vector of data values, or a list of numeric data
    vectors representing the respective samples, or fitted linear model
    objects (inheriting from class \code{"lm"}).}
  \item{g}{a vector or factor object giving the group for the
    corresponding elements of \code{x}.
    Ignored if \code{x} is a list.}
}
\details{
  If \code{x} is a list, its elements are taken as the samples or fitted
  linear models to be compared for homogeneity of variances.  In this
  case, the elements must either all be numeric data vectors or fitted
  linear model objects, \code{g} is ignored, and one can simply use
  \code{bartlett.test(x)} to perform the test.  If the samples are not
  yet contained in a list, use \code{bartlett.test(list(x, ...))}.

  Otherwise, \code{x} must be a numeric data vector, and \code{g} must
  be a vector or factor object of the same length as \code{x} giving the
  group for the corresponding elements of \code{x}.
}
\value{
  A list of class \code{"htest"} containing the following components:
  \item{statistic}{Bartlett's K-squared test statistic.}
  \item{parameter}{the degrees of freedom of the approximate chi-squared
    distribution of the test statistic.}
  \item{p.value}{the p-value of the test.}
  \item{method}{the string
    \code{"Bartlett test for homogeneity of variances"}.}
  \item{data.name}{a character string giving the names of the data.}
}
\seealso{
  \code{\link{var.test}} for the special case of comparing variances in
  two samples from normal distributions;
  \code{\link{fligner.test}} for a rank-based (nonparametric) k-sample
  test for homogeneity of variances;
  \code{\link{ansari.test}} and \code{\link{mood.test}} for two rank
  based two-sample tests for difference in scale.
}
\references{
  Bartlett, M. S. (1937).
  Properties of sufficiency and statistical tests.
  \emph{Proceedings of the Royal Statistical Society Series A}
  \bold{160}, 268--282.
}
\examples{
% FIXME: could use something more interesting here
data(InsectSprays)
plot(InsectSprays$count ~ InsectSprays$spray)
bartlett.test(InsectSprays$count, InsectSprays$spray)
}
\keyword{htest}
