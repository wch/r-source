% File src/library/stats/man/SignRank.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{SignRank}
\alias{SignRank}
\alias{dsignrank}
\alias{psignrank}
\alias{qsignrank}
\alias{rsignrank}
\title{Distribution of the Wilcoxon Signed Rank Statistic}
\description{
  Density, distribution function, quantile function and random
  generation for the distribution of the Wilcoxon Signed Rank statistic
  obtained from a sample with size \code{n}.
}
\usage{
dsignrank(x, n, log = FALSE)
psignrank(q, n, lower.tail = TRUE, log.p = FALSE)
qsignrank(p, n, lower.tail = TRUE, log.p = FALSE)
rsignrank(nn, n)
}
\arguments{
  \item{x, q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{nn}{number of observations. If \code{length(nn) > 1}, the length
    is taken to be the number required.}
  \item{n}{number(s) of observations in the sample(s).  A positive
    integer, or a vector of such integers.}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]}, otherwise, \eqn{P[X > x]}.}
}
\value{
  \code{dsignrank} gives the density,
  \code{psignrank} gives the distribution function,
  \code{qsignrank} gives the quantile function, and
  \code{rsignrank} generates random deviates.

  The length of the result is determined by \code{nn} for
  \code{rsignrank}, and is the maximum of the lengths of the
  numerical arguments for the other functions.  
  
  The numerical arguments other than \code{nn} are recycled to the
  length of the result.  Only the first elements of the logical
  arguments are used.
}
\details{
  This distribution is obtained as follows.  Let \code{x} be a sample of
  size \code{n} from a continuous distribution symmetric about the
  origin.  Then the Wilcoxon signed rank statistic is the sum of the
  ranks of the absolute values \code{x[i]} for which \code{x[i]} is
  positive.  This statistic takes values between \eqn{0} and
  \eqn{n(n+1)/2}, and its mean and variance are \eqn{n(n+1)/4} and
  \eqn{n(n+1)(2n+1)/24}, respectively.

  If either of the first two arguments is a vector, the recycling rule is
  used to do the calculations for all combinations of the two up to
  the length of the longer vector.
}
\author{Kurt Hornik; efficiency improvement by Ivo Ugrina.}
\seealso{
  \code{\link{wilcox.test}} to calculate the statistic from data, find p
  values and so on.

  \link{Distributions} for standard distributions, including
  \code{\link{dwilcox}} for the distribution of \emph{two-sample}
  Wilcoxon rank sum statistic.
}
\examples{
require(graphics)

par(mfrow = c(2,2))
for(n in c(4:5,10,40)) {
  x <- seq(0, n*(n+1)/2, length = 501)
  plot(x, dsignrank(x, n = n), type = "l",
       main = paste0("dsignrank(x, n = ", n, ")"))
}
\dontshow{
p <- c(1, 1, 1, 2, 2:6, 8, 10, 11, 13, 15, 17, 20, 22, 24,
       27, 29, 31, 33, 35, 36, 38, 39, 39, 40)
stopifnot(round(dsignrank(0:56, n = 10)* 2^10) == c(p, rev(p), 0),
          qsignrank((1:16)/ 16, n = 4) == c(0:2, rep(3:7, each = 2), 8:10))
}
}
\keyword{distribution}
