\name{Chisquare}
\title{The Chi-Square Distribution}
\usage{
dchisq(x, df)
pchisq(q, df)
qchisq(p, df)
rchisq(n, df)
}
\alias{dchisq}
\alias{pchisq}
\alias{qchisq}
\alias{rchisq}
\value{
These functions provide information about the chi-square distribution
with \code{df} degrees of freedom.  \code{dchisq} gives the density,
\code{pchisq} gives the distribution function \code{qchisq} gives
the quantile function and \code{rchisq} generates random deviates.

The chi-square distribution with \code{df} \eqn{= n} degrees of freedom has density
\deqn{f(x) =
\frac{1}{{2}^{n/2} \Gamma (n/2)}
{x}^{n/2-1} {e}^{-x/2}}{f(x) = 1 / (2^(n/2) Gamma(n/2))  x^(n/2-1) e^(-x/2)}
for \eqn{x > 0}.
}
\seealso{
\code{\link{dnchisq}}, \code{\link{dgamma}}, \code{\link{dexp}}.
}
\examples{
dchisq(1, df=1:3)
x <- 1:10
## Chisquare( df = 2) is a special exponential distribution
dchisq(x, df=2) == dexp(x, 1/2)
pchisq(x, df=2) == pexp(x, 1/2)#- only approximately
}
\keyword{distribution}
