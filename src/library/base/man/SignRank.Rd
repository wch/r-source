\name{SignRank}
\title{Distribution of the Wilcoxon Signed Rank Statistic}
\usage{
dsignrank(x, n)
psignrank(q, n)
qsignrank(p, n)
rsignrank(nn, n)
}
\alias{dsignrank}
\alias{psignrank}
\alias{qsignrank}
\alias{rsignrank}
\arguments{
    \item{x,q}{vector of quantiles.}
    \item{p}{vector of probabilities.}
    \item{nn}{number of observations to generate.}
    \item{n}{numbers of observations in the sample.  Must be positive
      	integers less than 50.}
}
\value{
    These functions provide information about the distribution of the
    Wilcoxon Signed Rank statistic obtained from a sample with size
    \code{n}.  \code{dsignrank} gives the density, \code{psignrank}
    gives the distribution function, \code{qsignrank} gives the quantile
    function, and \code{rsignrank} generates random deviates.

    This distribution is obtained as follows.  Let \code{x} be a sample
    of size \code{n} from a continuous distribution symmetric about the
    origin.  Then the Wilcoxon signed rank statistic is the sum of the
    ranks of the absolute values \code{x[i]} for which \code{x[i]} is
    positive.  This statistic takes values between \eqn{0} and
    \eqn{n(n+1)/2}, and its mean and variance are \eqn{n(n+1)/4} and
    \eqn{n(n+1)(2n+1)/24}, respectively.
}
\author{Kurt Hornik \email{hornik@ci.tuwien.ac.at}}
\seealso{\code{\link{dwilcox}} etc, for the \emph{two-sample} Wilcoxon
    rank sum statistic.}
\examples{
par(mfrow=c(2,2))
for(n in c(4:5,10,40)) {
  x <- seq(0, n*(n+1)/2, length=501)
  plot(x, dsignrank(x,n=n), type='l', main=paste("dsignrank(x,n=",n,")"))
}
}
\keyword{distribution}
