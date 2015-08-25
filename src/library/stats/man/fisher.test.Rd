% File src/library/stats/man/fisher.test.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{fisher.test}
\alias{fisher.test}
\title{Fisher's Exact Test for Count Data}
\description{
  Performs Fisher's exact test for testing the null of independence of
  rows and columns in a contingency table with fixed marginals.
}
\usage{
fisher.test(x, y = NULL, workspace = 200000, hybrid = FALSE,
            control = list(), or = 1, alternative = "two.sided",
            conf.int = TRUE, conf.level = 0.95,
            simulate.p.value = FALSE, B = 2000)
}
\arguments{
  \item{x}{either a two-dimensional contingency table in matrix form,
    or a factor object.}
  \item{y}{a factor object; ignored if \code{x} is a matrix.}
  \item{workspace}{an integer specifying the size of the workspace
    used in the network algorithm.  In units of 4 bytes.  Only used for
    non-simulated p-values larger than \eqn{2 \times 2}{2 by 2} tables.}
  \item{hybrid}{a logical. Only used for larger than \eqn{2 \times 2}{2 by 2}
    tables, in which cases it indicates whether the exact probabilities
    (default) or a hybrid approximation thereof should be computed.
    See \sQuote{Details}.}
  \item{control}{a list with named components for low level algorithm
    control.  At present the only one used is \code{"mult"}, a positive
    integer \eqn{\ge 2} with default 30 used only for larger than
    \eqn{2 \times 2}{2 by 2} tables.  This says how many times as much
    space should be allocated to paths as to keys: see file
    \file{fexact.c} in the sources of this package.}
  \item{or}{the hypothesized odds ratio.  Only used in the
    \eqn{2 \times 2}{2 by 2} case.}
  \item{alternative}{indicates the alternative hypothesis and must be
    one of \code{"two.sided"}, \code{"greater"} or \code{"less"}.
    You can specify just the initial letter.  Only used in the
    \eqn{2 \times 2}{2 by 2} case.}
  \item{conf.int}{logical indicating if a confidence interval for the
    odds ratio in a \eqn{2 \times 2}{2 by 2} table should be
    computed (and returned).}
  \item{conf.level}{confidence level for the returned confidence
    interval.  Only used in the \eqn{2 \times 2}{2 by 2} case and if
    \code{conf.int = TRUE}.}
  \item{simulate.p.value}{a logical indicating whether to compute
    p-values by Monte Carlo simulation, in larger than \eqn{2 \times
      2}{2 by 2} tables.}
  \item{B}{an integer specifying the number of replicates used in the
    Monte Carlo test.}
}
\value{
  A list with class \code{"htest"} containing the following components:
  \item{p.value}{the p-value of the test.}
  \item{conf.int}{a confidence interval for the odds ratio.
    Only present in the \eqn{2 \times 2}{2 by 2} case and if argument
    \code{conf.int = TRUE}.}
  \item{estimate}{an estimate of the odds ratio.  Note that the
    \emph{conditional} Maximum Likelihood Estimate (MLE) rather than the
    unconditional MLE (the sample odds ratio) is used.
    Only present in the \eqn{2 \times 2}{2 by 2} case.}
  \item{null.value}{the odds ratio under the null, \code{or}.
    Only present in the \eqn{2 \times 2}{2 by 2} case.}
  \item{alternative}{a character string describing the alternative
    hypothesis.}
  \item{method}{the character string
    \code{"Fisher's Exact Test for Count Data"}.}
  \item{data.name}{a character string giving the names of the data.}
}
\details{
  If \code{x} is a matrix, it is taken as a two-dimensional contingency
  table, and hence its entries should be nonnegative integers.
  Otherwise, both \code{x} and \code{y} must be vectors of the same
  length.  Incomplete cases are removed, the vectors are coerced into
  factor objects, and the contingency table is computed from these.

  For \eqn{2 \times 2}{2 by 2} cases, p-values are obtained directly
  using the (central or non-central) hypergeometric
  distribution. Otherwise, computations are based on a C version of the
  FORTRAN subroutine FEXACT which implements the network developed by
  Mehta and Patel (1986) and improved by Clarkson, Fan and Joe (1993).
  The FORTRAN code can be obtained from
  \url{http://www.netlib.org/toms/643}.  Note this fails (with an error
  message) when the entries of the table are too large.  (It transposes
  the table if necessary so it has no more rows than columns.  One
  constraint is that the product of the row marginals be less than
  \eqn{2^{31} - 1}{2^31 - 1}.)

  For \eqn{2 \times 2}{2 by 2} tables, the null of conditional
  independence is equivalent to the hypothesis that the odds ratio
  equals one.  \sQuote{Exact} inference can be based on observing that in
  general, given all marginal totals fixed, the first element of the
  contingency table has a non-central hypergeometric distribution with
  non-centrality parameter given by the odds ratio (Fisher, 1935).  The
  alternative for a one-sided test is based on the odds ratio, so
  \code{alternative = "greater"} is a test of the odds ratio being bigger
  than \code{or}.

  Two-sided tests are based on the probabilities of the tables, and take
  as \sQuote{more extreme} all tables with probabilities less than or
  equal to that of the observed table, the p-value being the sum of such
  probabilities.

  For larger than  \eqn{2 \times 2}{2 by 2} tables and \code{hybrid =
    TRUE}, asymptotic chi-squared probabilities are only used if the
  \sQuote{Cochran conditions} are satisfied, that is if no cell has
  count zero, and more than 80\% of the cells have counts at least 5:
  otherwise the exact calculation is used.

  Simulation is done conditional on the row and column marginals, and
  works only if the marginals are strictly positive.  (A C translation
  of the algorithm of Patefield (1981) is used.)
}
\references{
  Agresti, A. (1990)
  \emph{Categorical data analysis}.
  New York: Wiley.
  Pages 59--66.

  Agresti, A. (2002)
  \emph{Categorical data analysis}. Second edition.
  New York: Wiley.
  Pages 91--101.

  Fisher, R. A. (1935)
  The logic of inductive inference.
  \emph{Journal of the Royal Statistical Society Series A} \bold{98},
  39--54.

  Fisher, R. A. (1962)
  Confidence limits for a cross-product ratio.
  \emph{Australian Journal of Statistics} \bold{4}, 41.

  Fisher, R. A. (1970)
  \emph{Statistical Methods for Research Workers.}  Oliver & Boyd.

  Mehta, C. R. and Patel, N. R. (1986)
  Algorithm 643. FEXACT: A Fortran subroutine for Fisher's exact test
  on unordered \eqn{r*c} contingency tables.
  \emph{ACM Transactions on Mathematical Software}, \bold{12},
  154--161.

  Clarkson, D. B., Fan, Y. and Joe, H. (1993)
  A Remark on Algorithm 643: FEXACT: An Algorithm for Performing
  Fisher's Exact Test in \eqn{r \times c}{r x c} Contingency Tables.
  \emph{ACM Transactions on Mathematical Software}, \bold{19},
  484--488.

  Patefield, W. M. (1981)
  Algorithm AS159.  An efficient method of generating r x c tables
  with given row and column totals.
  \emph{Applied Statistics} \bold{30}, 91--97.
}
\seealso{
  \code{\link{chisq.test}}

  \code{fisher.exact} in package \CRANpkg{exact2x2} for alternative
  interpretations of two-sided tests and confidence intervals for
  \eqn{2 \times 2}{2 by 2} tables.
}
\examples{
## Agresti (1990, p. 61f; 2002, p. 91) Fisher's Tea Drinker
## A British woman claimed to be able to distinguish whether milk or
##  tea was added to the cup first.  To test, she was given 8 cups of
##  tea, in four of which milk was added first.  The null hypothesis
##  is that there is no association between the true order of pouring
##  and the woman's guess, the alternative that there is a positive
##  association (that the odds ratio is greater than 1).
TeaTasting <-
matrix(c(3, 1, 1, 3),
       nrow = 2,
       dimnames = list(Guess = c("Milk", "Tea"),
                       Truth = c("Milk", "Tea")))
fisher.test(TeaTasting, alternative = "greater")
## => p = 0.2429, association could not be established

## Fisher (1962, 1970), Criminal convictions of like-sex twins
Convictions <-
matrix(c(2, 10, 15, 3),
       nrow = 2,
       dimnames =
       list(c("Dizygotic", "Monozygotic"),
            c("Convicted", "Not convicted")))
Convictions
fisher.test(Convictions, alternative = "less")
fisher.test(Convictions, conf.int = FALSE)
fisher.test(Convictions, conf.level = 0.95)$conf.int
fisher.test(Convictions, conf.level = 0.99)$conf.int

## A r x c table  Agresti (2002, p. 57) Job Satisfaction
Job <- matrix(c(1,2,1,0, 3,3,6,1, 10,10,14,9, 6,7,12,11), 4, 4,
dimnames = list(income = c("< 15k", "15-25k", "25-40k", "> 40k"),
                satisfaction = c("VeryD", "LittleD", "ModerateS", "VeryS")))
fisher.test(Job)
fisher.test(Job, simulate.p.value = TRUE, B = 1e5)
}
\keyword{htest}
