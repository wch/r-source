\name{bartlett.test}
\alias{bartlett.test}
\title{Bartlett Test for Homogeneity of Variances}
\description{
  \code{bartlett.test} performs Bartlett's test of the null that the
  variances in each of the groups (samples) are the same.
}
\usage{
bartlett.test(x, g)
}
\arguments{
  \item{x}{a numeric vector of data values, or a list of numeric data
    vectors.}
  \item{g}{a vector or factor object giving the group for the
    corresponding elements of \code{x}.
    Ignored if \code{x} is a list.}
}
\value{
  A list of class \code{"htest"} containing the following components:
  \item{statistic}{Bartlett's K-square test statistic.}
  \item{parameter}{the degrees of freedom of the approximate chi-square
    distribution of the test statistic.}
  \item{p.value}{the p-value of the test.}
  \item{method}{the string
    \code{"Bartlett test for homogeneity of variances"}.}
  \item{data.name}{a character string giving the names of the data.}
}
\details{
  If \code{x} is a list, its elements are taken as the samples to be
  compared for homogeneity of variances, and hence have to be numeric
  data vectors.  In this case, \code{g} is ignored, and one can simply
  use \code{bartlett.test(x)} to perform the test.  If the samples are
  not yet contained in a list, use \code{bartlett.test(list(x, ...))}.

  Otherwise, \code{x} must be a numeric data vector, and \code{g} must
  be a vector or factor object of the same length as \code{x} giving the
  group for the corresponding elements of \code{x}.
}
\seealso{
  \code{\link{var.test}} for the special case of comparing variances in
  two samples from normal distributions;
  \code{\link{ansari.test}} and \code{\link{mood.test}} for two rank
  based (nonparametric) two-sample tests for difference in scale.
}
% FIXME: need a reference
\examples{
% FIXME: could use something more interesting here
data(InsectSprays)
plot(InsectSprays$count ~ InsectSprays$spray)
bartlett.test(InsectSprays$count, InsectSprays$spray)
}
\keyword{htest}
