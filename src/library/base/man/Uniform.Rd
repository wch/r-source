\name{Uniform}
\title{The Uniform Distribution}
\usage{
dunif(x, min=0, max=1)
punif(q, min=0, max=1)
qunif(p, min=0, max=1)
runif(n, min=0, max=1)
}
\alias{dunif}
\alias{punif}
\alias{qunif}
\alias{runif}
\arguments{
\item{x,q}{vector of quantiles.}
\item{p}{vector of probabilities.}
\item{n}{number of observations to generate.}
\item{min,max}{lower and upper limits of the distribution.}
}
\description{
These functions provide information about the uniform distribution
on the interval from \code{min} to \code{max}.  \code{dunif} gives the
density, \code{punif} gives the distribution function \code{qunif}
gives the quantile function and \code{runif} generates random
deviates.
}
\detail{
If \code{min} or \code{max} are not specified they assume the default
values of \code{0} and \code{1} respectively.

The uniform distribution has density
\deqn{f(x) = \frac{1}{max-min}}{f(x) = 1/(max-min)}
for \eqn{min \le x \le max}.

For the case of \eqn{u := min == max}, the limit case of
\eqn{X \equiv u}{X == u} is assumed.
}
\seealso{
  \code{\link{.Random.seed}} about random number generation,
  \code{\link{rnorm}}, etc for other distributions.
}
\examples{
u <- runif(20)
all(punif(u) == u) # T
all(dunif(u) == 1) # T

var(runif(10000))#- ~ = 1/12 = .08333
all(runif(100, 2,2) == 2)#-> TRUE [exhibits bug in R version <= 0.63.1]
}
\keyword{distribution}
