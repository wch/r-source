\name{Chisquare}
\title{The (non-central) Chi-Square Distribution}
\usage{
dchisq(x, df, ncp=0)
pchisq(q, df, ncp=0)
qchisq(p, df, ncp=0)
rchisq(n, df)
}
\alias{dchisq}
\alias{pchisq}
\alias{qchisq}
\alias{rchisq}
\arguments{
\item{x,q}{vector of quantiles.}
\item{p}{vector of probabilities.}
\item{n}{number of observations to generate.}
\item{df}{degrees of freedom.}
\item{ncp}{non-centrality parameter.}
}
\value{
  These functions provide information about the chi-square
  (\eqn{\chi^2}{chi^2}) distribution with \code{df} degrees of freedom and
  optional non-centrality parameter \code{ncp}.

  The chi-square distribution with \code{df}\eqn{= n} degrees of freedom
  has density
  \deqn{f_n(x) = \frac{1}{{2}^{n/2} \Gamma (n/2)} {x}^{n/2-1} {e}^{-x/2}}{%
        f_n(x) = 1 / (2^(n/2) Gamma(n/2))  x^(n/2-1) e^(-x/2)}
  for \eqn{x > 0}.

  \code{dchisq} gives the density \eqn{f_n},
  \code{pchisq} gives the distribution function \eqn{F_n}, \code{qchisq} gives
  the quantile function and \code{rchisq} generates random deviates.

  The non-central chi-square distribution with \code{df}\eqn{= n} degrees of
  freedom and non-centrality parameter \code{ncp} \eqn{= \lambda} has density
  \deqn{
    f(x) = e^{-\lambda / 2}
      \sum_{r=0}^\infty \frac{(\lambda/2)^r}{r!}\, f_{n + 2r}(x)}{%
    f(x) = exp(-lambda/2) SUM_{r=0}^infty ((lambda/2)^r / r!) dchisq(x, df + 2r)
  }
  for \eqn{x \ge 0}.
}
\seealso{
%DEPRECATED: \code{\link{dnchisq}} for the non-central chi-square distribution,
\code{\link{dgamma}} for the gamma distribution which generalizes
the chi-square one.
}
\examples{
dchisq(1, df=1:3)
pchisq(1, df= 3)
pchisq(1, df= 3, ncp = 0:4)# includes the above

x <- 1:10
## Chisquare( df = 2) is a special exponential distribution
dchisq(x, df=2) == dexp(x, 1/2)
pchisq(x, df=2) == pexp(x, 1/2)#- only approximately
}
\keyword{distribution}
