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
\description{
These functions provide information about the uniform distribution
on the interval from \code{min} to \code{max}.  \code{dunif} gives the
density, \code{punif} gives the distribution function \code{qunif}
gives the quantile function and \code{runif} generates random
deviates.

If \code{min} or \code{max} are not specified they assume the default
values of \code{0} and \code{1} respectively.

The uniform distribution has density
\deqn{f(x) = \frac{1}{max-min}}{f(x) = 1/(max-min)}
for \eqn{min \le x \le max}.
}
\seealso{
\code{\link{.Random.seed}}, \code{\link{rnorm}}, \ldots.
}
\examples{
var(runif(10000))#- ~ = 1/12 = .08333
}
\keyword{distribution}
