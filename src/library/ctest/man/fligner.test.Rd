\name{fligner.test}
\alias{fligner.test}
\title{Fligner-Killeen Test for Homogeneity of Variances}
\description{
  Performs a Fligner-Killeen (median) test of the null that the
  variances in each of the groups (samples) are the same.
}
\usage{fligner.test(x, g)}
\arguments{
  \item{x}{a numeric vector of data values, or a list of numeric data
    vectors.}
  \item{g}{a vector or factor object giving the group for the
    corresponding elements of \code{x}.
    Ignored if \code{x} is a list.}
}
\details{
  If \code{x} is a list, its elements are taken as the samples to be
  compared for homogeneity of variances, and hence have to be numeric
  data vectors.  In this case, \code{g} is ignored, and one can simply
  use \code{fligner.test(x)} to perform the test.  If the samples are
  not yet contained in a list, use \code{fligner.test(list(x, ...))}.

  Otherwise, \code{x} must be a numeric data vector, and \code{g} must
  be a vector or factor object of the same length as \code{x} giving the
  group for the corresponding elements of \code{x}.
  
  The Fligner-Killeen (median) test has been determined in a simulation
  study as one of the many tests for homogeneity of variances which is
  most robust against departures from normality, see Conover, Johnson &
  Johnson (1981).  It is a k-sample simple linear rank which uses the
  ranks of the absolute values of the centered samples and weights
  \eqn{a(i) = qnorm((1 + i/(n+1))/2)}.  The version implemented here
  uses median centering in each of the samples (F-K:med \eqn{X^2} in the
  reference).
}
\value{
  A list of class \code{"htest"} containing the following components:
  \item{statistic}{the Fligner-Killeen:med \eqn{X^2} test statistic.}
  \item{parameter}{the degrees of freedom of the approximate chi-squared
    distribution of the test statistic.}
  \item{p.value}{the p-value of the test.}
  \item{method}{the string
    \code{"Fligner-Killeen test for homogeneity of variances"}.}
  \item{data.name}{a character string giving the names of the data.}
}
\references{
  W. J. Conover & Mark E. Johnson & Myrie M. Johnson (1981).
  A comparative study of tests for homogeneity of variances, with
  applications to the outer continental shelf bidding data.
  \emph{Technometrics} \bold{23}, 351--361.
}
\seealso{
  \code{\link{ansari.test}} and \code{\link{mood.test}} for rank-based
  two-sample test for a difference in scale parameters;
  \code{\link{var.test}} and \code{\link{bartlett.test}} for parametric
  tests for the homogeneity of variances.
}
\examples{
% FIXME: could use something more interesting here
data(InsectSprays)
plot(InsectSprays$count ~ InsectSprays$spray)
fligner.test(InsectSprays$count, InsectSprays$spray)
## Compare this to bartlett.test()
}
\keyword{htest}
