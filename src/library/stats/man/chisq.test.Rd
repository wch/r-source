\name{chisq.test}
\alias{chisq.test}
\concept{goodness-of-fit}
\title{Pearson's Chi-squared Test for Count Data}
\description{
  \code{chisq.test} performs chi-squared contingency table tests
                    and goodness-of-fit tests.
}
\usage{
chisq.test(x, y = NULL, correct = TRUE,
           p = rep(1/length(x), length(x)), rescale.p = FALSE,
           simulate.p.value = FALSE, B = 2000)
}
\arguments{
  \item{x}{a vector or matrix.}
  \item{y}{a vector; ignored if \code{x} is a matrix.}
  \item{correct}{a logical indicating whether to apply continuity
    correction when computing the test statistic for 2x2 tables: one
    half is subtracted from all \eqn{|O-E|} differences.  No correction
    is done if \code{simulate.p.value = TRUE}.}
  \item{p}{a vector of probabilities of the same length of \code{x}.
    An error is given if any entry of \code{p} is negative.}
  \item{rescale.p}{a logical scalar; if TRUE then \code{p} is rescaled
    (if necessary) to sum to 1.  If \code{rescale.p} is FALSE, and
    \code{p} does not sum to 1, an error is given.}
  \item{simulate.p.value}{a logical indicating whether to compute
    p-values by Monte Carlo simulation.}
  \item{B}{an integer specifying the number of replicates used in the
    Monte Carlo test.}
}
\details{
  If \code{x} is a matrix with one row or column, or if \code{x} is a
  vector and \code{y} is not given, then a \dQuote{goodness-of-fit test}
  is performed (\dQuote{\code{x} is treated as a one-dimensional
  contingency table}).  The entries of \code{x} must be non-negative
  integers.  In this case, the hypothesis tested is whether the
  population probabilities equal those in \code{p}, or are all equal if
  \code{p} is not given.

  If \code{x} is a matrix with at least two rows and columns, it is
  taken as a two-dimensional contingency table.  Again, the entries
  of \code{x} must be non-negative integers.  Otherwise, \code{x} and
  \code{y} must be vectors or factors of the same length; incomplete
  cases are removed, the objects are coerced into factor objects, and
  the contingency table is computed from these.  Then, Pearson's
  chi-squared test of the null hypothesis that the joint distribution of
  the cell counts in a 2-dimensional contingency table is the product of
  the row and column marginals is performed.

  If \code{simulate.p.value} is \code{FALSE}, the p-value is computed
  from the asymptotic chi-squared distribution of the test statistic;
  continuity correction is only used in the 2-by-2 case (if \code{correct}
  is \code{TRUE}, the default).  Otherwise the p-value is computed for a
  Monte Carlo test (Hope, 1968) with \code{B} replicates.

  In the contingency table case simulation is done by random sampling
  from the set of all contingency tables with given marginals, and works
  only if the marginals are strictly positive.  (A C translation of the
  algorithm of Patefield (1981) is used.)  Continuity correction is
  never used, and the statistic is quoted without it.  Note that this is
  not the usual sampling situation for the chi-squared test but rather
  that for Fisher's exact test.

  In the goodness-of-fit case simulation is done by random sampling from
  the discrete distribution specified by \code{p}, each sample being
  of size \code{n = sum(x)}.  This simulation is done in \code{R}
  and may be slow.
}
\value{
  A list with class \code{"htest"} containing the following
  components:
  \item{statistic}{the value the chi-squared test statistic.}
  \item{parameter}{the degrees of freedom of the approximate
    chi-squared distribution of the test statistic, \code{NA} if the
    p-value is computed by Monte Carlo simulation.}
  \item{p.value}{the p-value for the test.}
  \item{method}{a character string indicating the type of test
    performed, and whether Monte Carlo simulation or continuity
    correction was used.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{observed}{the observed counts.}
  \item{expected}{the expected counts under the null hypothesis.}
  \item{residuals}{the Pearson residuals, \code{(observed - expected)
      / sqrt(expected)}.}
}
\references{
  Hope, A. C. A. (1968)
  A simplified Monte Carlo significance test procedure.
  \emph{J. Roy, Statist. Soc. B} \bold{30}, 582--598.
  
  Patefield, W. M. (1981)
  Algorithm AS159.  An efficient method of generating r x c tables
  with given row and column totals.
  \emph{Applied Statistics} \bold{30}, 91--97.
}
\examples{
## Not really a good example
chisq.test(InsectSprays$count > 7, InsectSprays$spray)
				# Prints test summary
chisq.test(InsectSprays$count > 7, InsectSprays$spray)$observed
				# Counts observed
chisq.test(InsectSprays$count > 7, InsectSprays$spray)$expected
				# Counts expected under the null

## Effect of simulating p-values
x <- matrix(c(12, 5, 7, 7), ncol = 2)
chisq.test(x)$p.value		# 0.4233
chisq.test(x, simulate.p.value = TRUE, B = 10000)$p.value
				# around 0.29!

## Testing for population probabilities
## Case A. Tabulated data
x <- c(A = 20, B = 15, C = 25)
chisq.test(x)
chisq.test(as.table(x))		# the same
x <- c(89,37,30,28,2)
p <- c(40,20,20,15,5)
try(
chisq.test(x, p = p)		# gives an error
)
chisq.test(x, p = p, rescale.p = TRUE)
				# works
p <- c(0.40,0.20,0.20,0.19,0.01)
				# Expected count in category 5
				# is 1.86 < 5 ==> chi square approx.
chisq.test(x, p = p)            # 	        maybe doubtful, but is ok!
chisq.test(x, p = p,simulate.p.value = TRUE)

## Case B. Raw data
x <- trunc(5 * runif(100))
chisq.test(table(x))            # NOT 'chisq.test(x)'!
}
\keyword{htest}
\keyword{distribution}
