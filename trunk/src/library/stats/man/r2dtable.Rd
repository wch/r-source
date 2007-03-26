\name{r2dtable}
\alias{r2dtable}
\title{Random 2-way Tables with Given Marginals}
\description{
  Generate random 2-way tables with given marginals using Patefield's
  algorithm.
}
\usage{
r2dtable(n, r, c)
}
\arguments{
  \item{n}{a non-negative numeric giving the number of tables to be
    drawn.}
  \item{r}{a non-negative vector of length at least 2 giving the row
    totals, to be coerced to \code{integer}.  Must sum to the same as
    \code{c}.}
  \item{c}{a non-negative vector of length at least 2 giving the column
    totals, to be coerced to \code{integer}.}
}
\value{
  A list of length \code{n} containing the generated tables as its
  components.
}
\references{
  Patefield, W. M. (1981)
  Algorithm AS159.  An efficient method of generating r x c tables
  with given row and column totals.
  \emph{Applied Statistics} \bold{30}, 91--97.
} 
\examples{
## Fisher's Tea Drinker data.
TeaTasting <-
matrix(c(3, 1, 1, 3),
       nr = 2,
       dimnames = list(Guess = c("Milk", "Tea"),
                       Truth = c("Milk", "Tea")))
## Simulate permutation test for independence based on the maximum
## Pearson residuals (rather than their sum).
rowTotals <- rowSums(TeaTasting)
colTotals <- colSums(TeaTasting)
nOfCases <- sum(rowTotals)
expected <- outer(rowTotals, colTotals, "*") / nOfCases
maxSqResid <- function(x) max((x - expected) ^ 2 / expected)
simMaxSqResid <-
    sapply(r2dtable(1000, rowTotals, colTotals), maxSqResid)
sum(simMaxSqResid >= maxSqResid(TeaTasting)) / 1000
## Fisher's exact test gives p = 0.4857 ...
}
\keyword{distribution}
