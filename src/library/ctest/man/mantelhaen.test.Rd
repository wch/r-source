\name{mantelhaen.test}
\alias{mantelhaen.test}
\title{Mantel-Haenszel Chi-Square Test for Count Data}
\description{
  \code{mantelhaen.test} performs a Mantel-Haenszel chi-square test of
  the null that \code{x} and \code{y} are conditionally independent in
  each stratum.
}
\usage{
mantelhaen.test(x, y = NULL, z = NULL, correct = TRUE)
}
\arguments{
  \item{x}{either an array of dimension 2 by 2 by s, where s is the
    number of strata, or a dichotomous factor object.}
  \item{y}{a dichotomous factor object;  ignored if x is an array.}
  \item{z}{a factor object idenitifying to which stratum the
    corresponding elements in \code{x} and \code{y} belong;
    ignored if \code{x} is an array.}
  \item{correct}{a logical indicating whether to apply continuity
    correction when computing the test statistic.}
}
\value{
  A list with class \code{"htest"} containing the following components:
  \item{statistic}{the Mantel-Haenszel chi-square statistic.}
  \item{parameter}{always 1, the degrees of freedom of the approximate
    chi-square distribution of the test statistic.}
  \item{p.value}{the p-value of the test.}
  \item{method}{a string indicating the method employed, and whether
    or not continuity correction was used.}
  \item{data.name}{a character string giving the names of the data.}
}
\details{
  If \code{x} is an array, it must be of dimension 2 by 2 by s, and
  the entries should be nonnegative integers.  \code{NA}'s are not
  allowed.  Otherwise, \code{x}, \code{y} and \code{z} must have the
  same length.  Triples containing \code{NA}'s are removed.  Both
  \code{x} and \code{y} must be dichotomous (take exactly 2 values).
}
\references{
  Alan Agresti (1990).
  \emph{Categorical data analysis}.
  New York: Wiley.
  Pages 230--234.
}
\examples{
data(UCBAdmissions)
mantelhaen.test(UCBAdmissions)
## No evidence for association between admission and gender
## when adjusted for department
}  
\keyword{htest}
