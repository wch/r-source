\name{mantelhaen.test}
\alias{mantelhaen.test}
\title{Cochran-Mantel-Haenszel Chi-Square Test for Count Data}
\description{
  Performs a Cochran-Mantel-Haenszel chi-square test of the null that
  two nominal variables are conditionally independent in each stratum.
}
\usage{
mantelhaen.test(x, y = NULL, z = NULL, correct = TRUE)
}
\arguments{
  \item{x}{either a 3-dimensional contingency table in array form where
    each dimension is at least 2 and the last dimension corresponds to
    the strata, or a factor object with at least 2 levels.}
  \item{y}{a factor object with at least 2 levels; ignored if \code{x}
    is an array.}
  \item{z}{a factor object with at least 2 levels identifying to which
    stratum the corresponding elements in \code{x} and \code{y} belong;
    ignored if \code{x} is an array.}
  \item{correct}{a logical indicating whether to apply continuity
    correction when computing the test statistic.}
}
\value{
  A list with class \code{"htest"} containing the following components:
  \item{statistic}{the Mantel-Haenszel chi-square statistic in the
    classical case of a 2 by 2 by \eqn{K} table (i.e., of dichotomous
    underlying variables); otherwise, the generalized
    Cochran-Mantel-Haenszel statistic.}
  \item{parameter}{the degrees of freedom of the approximate chi-square
    distribution of the test statistic (\eqn{1} in the classical case).}
  \item{p.value}{the p-value of the test.}
  \item{method}{a string indicating the method employed, and whether
    or not continuity correction was used.}
  \item{data.name}{a character string giving the names of the data.}
}
\details{
  If \code{x} is an array, each dimension must be at least 2, and
  the entries should be nonnegative integers.  \code{NA}'s are not
  allowed.  Otherwise, \code{x}, \code{y} and \code{z} must have the
  same length.  Triples containing \code{NA}'s are removed.  All
  variables must take at least two different values.
}
\note{
  The asymptotic distribution is only valid if there is no three-way
  interaction.
}
\references{
  Alan Agresti (1990).
  \emph{Categorical data analysis}.
  New York: Wiley.
  Pages 230--235.
}
\examples{
data(UCBAdmissions)
mantelhaen.test(UCBAdmissions)
## No evidence for association between admission and gender
## when adjusted for department
}  
\keyword{htest}
