\name{NegBinomial}
\title{The Negative Binomial Distribution}
\usage{
dnbinom(x, n, p)
pnbinom(q, n, p)
qnbinom(prob, n, p)
rnbinom(nobs, n, p)
}
\alias{dnbinom}
\alias{pnbinom}
\alias{qnbinom}
\alias{rnbinom}
\description{
  These functions provide information about the negative binomial
  distribution with parameters \code{n} and \code{p}.  \code{dnbinom}
  gives the density, \code{pnbinom} gives the distribution function, 
  \code{qnbinom} gives the quantile function and \code{rnbinom}
  generates random deviates.

  The negative binomial distribution has density
  \deqn{
    p(x) = {x+n-1 \choose x} p^n (1-p)^x}{
    p(x) = Choose(x+n-1,x) p^n (1-p)^x}
  for \eqn{x = 0, 1, 2, \ldots}
}
\seealso{
  \code{\link{dbinom}} for the binomial and \code{\link{dpois}} for the
  Poisson distribution.
}
\examples{
x <- 0:11
dnbinom(x, n = 1, p = 1/2) * 2^(1 + x) # == 1
126 /  dnbinom(0:8, n = 2, p = 1/2) #- theoretically integer
## Cumulative ('p') = Sum of discrete prob.s ('d');  Relative error 
summary(1 - cumsum(dnbinom(x, n = 2, p = 1/2)) / pnbinom(x, n = 2, p = 1/2))
}
\keyword{distribution}
