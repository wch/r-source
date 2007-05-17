\name{mcnemar.test}
\alias{mcnemar.test}
\title{McNemar's Chi-squared Test for Count Data}
\description{
  Performs McNemar's chi-squared test for symmetry of rows and columns
  in a two-dimensional contingency table.
}
\usage{
mcnemar.test(x, y = NULL, correct = TRUE)
}
\arguments{
  \item{x}{either a two-dimensional contingency table in matrix form,
    or a factor object.}
  \item{y}{a factor object; ignored if \code{x} is a matrix.}
  \item{correct}{a logical indicating whether to apply continuity
    correction when computing the test statistic.} 
}
\value{
  A list with class \code{"htest"} containing the following components:
  \item{statistic}{the value of McNemar's statistic.}
  \item{parameter}{the degrees of freedom of the approximate
    chi-squared distribution of the test statistic.}
  \item{p.value}{the p-value of the test.}
  \item{method}{a character string indicating the type of test
    performed, and whether continuity correction was used.}
  \item{data.name}{a character string giving the name(s) of the data.}
}
\details{
  The null is that the probabilities of being classified into cells
  \code{[i,j]} and \code{[j,i]} are the same.

  If \code{x} is a matrix, it is taken as a two-dimensional contingency
  table, and hence its entries should be nonnegative integers.
  Otherwise, both \code{x} and \code{y} must be vectors of the same
  length.  Incomplete cases are removed, the vectors are coerced into
  factor objects, and the contingency table is computed from these.

  Continuity correction is only used in the 2-by-2 case if
  \code{correct} is \code{TRUE}.
}
\references{
  Alan Agresti (1990).
  \emph{Categorical data analysis}.
  New York: Wiley.
  Pages 350--354.
}
\examples{
## Agresti (1990), p. 350.
## Presidential Approval Ratings.
##  Approval of the President's performance in office in two surveys,
##  one month apart, for a random sample of 1600 voting-age Americans.
Performance <-
matrix(c(794, 86, 150, 570),
       nrow = 2,
       dimnames = list("1st Survey" = c("Approve", "Disapprove"),
                       "2nd Survey" = c("Approve", "Disapprove")))
Performance
mcnemar.test(Performance)
## => significant change (in fact, drop) in approval ratings
}
\keyword{htest}
