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
\author{Kurt Hornik \email{hornik@ci.tuwien.ac.at}}
\seealso{}
\examples{
x <- -1:(4*6 + 1)
fx <- dwilcox(x, 4, 6)
all(fx == dwilcox(x, 6, 4))
Fx <- pwilcox(x, 4, 6)
all(abs(Fx - cumsum(fx)) < 10 * .Machine$double.eps)

layout(rbind(1,2),width=1,heights=c(3,2))
plot(x, fx,type='h', col="violet",
     main= "Probabilities (density) of Wilcoxon-Statist.(n=6,m=4)")
plot(x, Fx,type="s", col="blue",
     main= "Distribution of Wilcoxon-Statist.(n=6,m=4)")
abline(h=0:1, col="gray20",lty=2)
layout(1)# set back

N <- 200
hist(U <- rwilcox(N, m=4,n=6), breaks=0:25 - 1/2, border="red", col="pink",
     sub = paste("N =",N))
mtext("N * f(x),  f() = true ``density''", side=3, col="blue")
 lines(x, N*fx, type='h', col='blue', lwd=2)
points(x, N*fx, cex=2)

## Better is a Quantile-Quantile Plot
qqplot(U, qw <- qwilcox((1:N - 1/2)/N, m=4,n=6),
       main = paste("Q-Q-Plot of empirical and theoretical quantiles",
                     "Wilcoxon Statistic,  (m=4, n=6)",sep="\n"))
n <- as.numeric(names(print(tU <- table(U))))
text(n+.2, n+.5, labels=tU, col="red")
}
\keyword{distribution}
