\name{Wilcoxon}
\title{Distribution of the Wilcoxon Rank Sum Statistic}
\usage{
dwilcox(x, m, n)
pwilcox(q, m, n)
qwilcox(p, m, n)
rwilcox(nn, m, n)
}
\alias{dwilcox}
\alias{pwilcox}
\alias{qwilcox}
\alias{rwilcox}
\arguments{
  \item{x,q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{nn}{number of observations to generate.}
  \item{m,n}{numbers of observations in the first and second sample,
    respectively.  Must be positive integers less than 50.}
}
\value{
  These functions provide information about the distribution of the
  Wilcoxon rank sum statistic obtained from samples with size \code{m}
  and \code{n}, respectively.  \code{dwilcox} gives the density,
  \code{pwilcox} gives the distribution function, \code{qwilcox} gives
  the quantile function, and \code{rwilcox} generates random deviates.

  This distribution is obtained as follows.  Let \code{x} and \code{y}
  be two random, independent samples of size \code{m} and \code{n}.
  Then the Wilcoxon rank sum statistic is the number of all pairs
  \code{(x[i], y[j])} for which \code{y[j]} is not greater than
  \code{x[i]}.  This statistic takes values between \code{0} and
  \code{m * n}, and its mean and variance are \code{m * n / 2} and
  \code{m * n * (m + n + 1) / 12}, respectively.
}
\keyword{distribution}
