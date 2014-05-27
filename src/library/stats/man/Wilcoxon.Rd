% File src/library/stats/man/Wilcoxon.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{Wilcoxon}
\alias{Wilcoxon}
\alias{dwilcox}
\alias{pwilcox}
\alias{qwilcox}
\alias{rwilcox}
\title{Distribution of the Wilcoxon Rank Sum Statistic}
\description{
  Density, distribution function, quantile function and random
  generation for the distribution of the Wilcoxon rank sum statistic
  obtained from samples with size \code{m} and \code{n}, respectively.
}
\usage{
dwilcox(x, m, n, log = FALSE)
pwilcox(q, m, n, lower.tail = TRUE, log.p = FALSE)
qwilcox(p, m, n, lower.tail = TRUE, log.p = FALSE)
rwilcox(nn, m, n)
}
\arguments{
  \item{x, q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{nn}{number of observations. If \code{length(nn) > 1}, the length
    is taken to be the number required.}
  \item{m, n}{numbers of observations in the first and second sample,
    respectively.  Can be vectors of positive integers.}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]}, otherwise, \eqn{P[X > x]}.}
}
\value{
  \code{dwilcox} gives the density,
  \code{pwilcox} gives the distribution function,
  \code{qwilcox} gives the quantile function, and
  \code{rwilcox} generates random deviates.

  The length of the result is determined by \code{nn} for
  \code{rwilcox}, and is the maximum of the lengths of the
  numerical arguments for the other functions.  
  
  The numerical arguments other than \code{nn} are recycled to the
  length of the result.  Only the first elements of the logical
  arguments are used.
}
\details{
  This distribution is obtained as follows.  Let \code{x} and \code{y}
  be two random, independent samples of size \code{m} and \code{n}.
  Then the Wilcoxon rank sum statistic is the number of all pairs
  \code{(x[i], y[j])} for which \code{y[j]} is not greater than
  \code{x[i]}.  This statistic takes values between \code{0} and
  \code{m * n}, and its mean and variance are \code{m * n / 2} and
  \code{m * n * (m + n + 1) / 12}, respectively.

  If any of the first three arguments are vectors, the recycling rule is
  used to do the calculations for all combinations of the three up to
  the length of the longest vector.
}
\note{
  S-PLUS uses a different (but equivalent) definition of the Wilcoxon
  statistic: see \code{\link{wilcox.test}} for details.
}
\section{Warning}{
  These functions can use large amounts of memory and stack (and even crash
  \R if the stack limit is exceeded and stack-checking is not in place)
  if one sample is large (several thousands or more).
}
\source{
  These are calculated via recursion, based on \code{cwilcox(k, m, n)},
  the number of choices with statistic \code{k} from samples of size
  \code{m} and \code{n}, which is itself calculated recursively and the
  results cached.  Then \code{dwilcox} and \code{pwilcox} sum
  appropriate values of \code{cwilcox}, and \code{qwilcox} is based on
  inversion.

  \code{rwilcox} generates a random permutation of ranks and evaluates
  the statistic.
}
\author{Kurt Hornik}
\seealso{
  \code{\link{wilcox.test}} to calculate the statistic from data, find p
  values and so on.

  \link{Distributions} for standard distributions, including
  \code{\link{dsignrank}} for the distribution of the
  \emph{one-sample} Wilcoxon signed rank statistic.
}
\examples{
require(graphics)

x <- -1:(4*6 + 1)
fx <- dwilcox(x, 4, 6)
Fx <- pwilcox(x, 4, 6)

layout(rbind(1,2), widths = 1, heights = c(3,2))
plot(x, fx, type = "h", col = "violet",
     main =  "Probabilities (density) of Wilcoxon-Statist.(n=6, m=4)")
plot(x, Fx, type = "s", col = "blue",
     main =  "Distribution of Wilcoxon-Statist.(n=6, m=4)")
abline(h = 0:1, col = "gray20", lty = 2)
layout(1) # set back

N <- 200
hist(U <- rwilcox(N, m = 4,n = 6), breaks = 0:25 - 1/2,
     border = "red", col = "pink", sub = paste("N =",N))
mtext("N * f(x),  f() = true \"density\"", side = 3, col = "blue")
 lines(x, N*fx, type = "h", col = "blue", lwd = 2)
points(x, N*fx, cex = 2)

## Better is a Quantile-Quantile Plot
qqplot(U, qw <- qwilcox((1:N - 1/2)/N, m = 4, n = 6),
       main = paste("Q-Q-Plot of empirical and theoretical quantiles",
                     "Wilcoxon Statistic,  (m=4, n=6)", sep = "\n"))
n <- as.numeric(names(print(tU <- table(U))))
text(n+.2, n+.5, labels = tU, col = "red")
}
\keyword{distribution}
