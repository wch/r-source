\name{F}
\title{The F Distribution}
\usage{
df(x, df1, df2)
pf(q, df1, df2, ncp=0)
qf(p, df1, df2)
rf(n, df1, df2)
}
\alias{df}
\alias{pf}
\alias{qf}
\alias{rf}
\arguments{
\item{x,q}{vector of quantiles.}
\item{p}{vector of probabilities.}
\item{n}{number of observations to generate.}
\item{df1,df2}{degrees of freedom.}
\item{ncp}{non-centrality parameter.}
}
\value{
  These functions provide information about the F distribution with
  \code{df1} and \code{df2} degrees of freedom (and optional non-centrality
  parameter \code{ncp}).
  \code{df} gives the density,
  \code{pf} gives the distribution function
  \code{qf} gives the quantile function
  and
  \code{rf} generates random deviates.

  The F distribution with \code{df1 =} \eqn{n_1}{n1} and \code{df2 =}
  \eqn{n_2}{n2} degrees of freedom has density
  \deqn{
    f(x) = \frac{\Gamma(n_1/2 + n_2/2)}{\Gamma(n_1/2)\Gamma(n_2/2)}
    \left(\frac{n_1}{n_2}\right)^{n_1/2} x^{n_1/2 -1}
    \left(1 + \frac{n_1 x}{n_2}\right)^{-(n_1 + n_2) / 2}%
  }{f(x) = Gamma((n1 + n2)/2) / (Gamma(n1/2) Gamma(n2/2))
    (n1/n2)^(n1/2) x^(n1/2 - 1)
    (1 + (n1/n2) x)^-(n1 + n2)/2}
  for \eqn{x > 0}.
}
\seealso{
  \code{\link{dt}} for Student's t distribution, the square of which is
  (almost) equivalent to the F distribution with \code{df2} \eqn{= 1}.
}
\examples{
df(1,1,1) == dt(1,1)# TRUE

## Identity:  qf(2*p -1, 1, df)) == qt(p, df)^2)  for  p >= 1/2
p <- seq(1/2, .99, length=50); df <- 10
rel.err <- function(x,y) ifelse(x==y,0, abs(x-y)/mean(abs(c(x,y))))
quantile(rel.err(qf(2*p -1, df1=1, df2=df), qt(p, df)^2), .90)# ~= 7e-9
}
\keyword{distribution}
