\name{fisher.test}
\alias{fisher.test}
\title{Fisher's Exact Test for Count Data}
\description{
  Performs Fisher's exact test for testing the null of independence of
  rows and columns in a contingency table with fixed marginals.
}
\usage{
fisher.test(x, y, alternative = "two.sided", workspace = 200000, 
            hybrid = FALSE)
}
\arguments{
  \item{x}{either a two-dimensional contingency table in matrix form,
    or a factor object.}
  \item{y}{a factor object; ignored if \code{x} is a matrix.} 
  \item{alternative}{indicates the alternative hypothesis and must be
    one of \code{"two.sided"}, \code{"greater"} or \code{"less"}.
    You can specify just the initial letter.  Only used in the 2 by
    2 case.}
  \item{workspace}{an integer specifying the size of the workspace
    used in the network algorithm.}
  \item{hybrid}{a logical indicating whether the exact probabilities
    (default) or a hybrid approximation thereof should be computed.
    In the hybrid case, asymptotic chi-square probabilies are only
    used provided that the ``Cochran'' conditions are satisfied.}
}
\value{
  A list with class \code{"htest"} containing the following components:
  \item{p.value}{the p-value of the test.}
  \item{alternative}{a character string describing the alternative
    hypothesis.} 
  \item{method}{the string \code{"Fisher's Exact Test for Count Data"}.}
  \item{data.name}{a character string giving the names of the data.}
}
\details{
  If \code{x} is a matrix, it is taken as a two-dimensional contingency
  table, and hence its entries should be nonnegative integers.
  Otherwise, both \code{x} and \code{y} must be vectors of the same
  length.  Incomplete cases are removed, the vectors are coerced into
  factor objects, and the contingency table is computed from these.

  In the one-sided 2 by 2 cases, p-values are obtained directly using
  the hypergeometric distribution.  Otherwise, computations are based on
  a C version of the FORTRAN subroutine FEXACT which implements the
  network developed by Mehta and Patel (1986) and improved by Clarkson,
  Fan & Joe (1993).  The FORTRAN code can be obtained from
  \url{http://www.netlib.org/toms/643}.
}
\references{
  Alan Agresti (1990).
  \emph{Categorical data analysis}.
  New York: Wiley.
  Pages 59--66.

  Cyrus R. Mehta & Nitin R. Patel (1986).
  Algorithm 643. FEXACT: A Fortran subroutine for Fisher's exact test
  on unordered \eqn{r*c} contingency tables.
  \emph{ACM Transactions on Mathematical Software}, \bold{12},
  154--161.
    
  Douglas B. Clarkson, Yuan-an Fan & Harry Joe (1993).
  A Remark on Algorithm 643: FEXACT: An Algorithm for Performing
  Fisher's Exact Test in \eqn{r \times c}{r x c} Contingency Tables.
  \emph{ACM Transactions on Mathematical Software}, \bold{19},
  484--488.
}
\seealso{
  \code{\link{chisq.test}}
}
\examples{
## Agresti (1990), p. 61f, Fisher's Tea Drinker
## A British woman claimed to be able to distinguish whether milk or
##  tea was added to the cup first.  To test, she was given 8 cups of
##  tea, in four of which milk was added first.  The null hypothesis
##  is that there is no association between the true order of pouring
##  and the women's guess, the alternative that there is a positive
##  association (that the odds ratio is greater than 1).
TeaTasting <-
matrix(c(3, 1, 1, 3),
       nr = 2,
       dimnames = list(Guess = c("Milk", "Tea"),
                       Truth = c("Milk", "Tea")))
fisher.test(TeaTasting, alternative = "greater")
## => p=0.2429, association could not be established
}
\keyword{htest}
