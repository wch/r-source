\name{T}
\title{The Student t Distribution}
\usage{
dt(x, df)
pt(q, df)
qt(p, df)
rt(n, df)
}
\alias{dt}
\alias{pt}
\alias{qt}
\alias{rt}
\description{
These functions provide information about the t distribution with
\code{df} degrees of freedom.  \code{dt} gives the density, \code{pt}
gives the distribution function \code{qt} gives the quantile function
and \code{rt} generates random deviates.

The t distribution with \eqn{n} degrees of freedom has density
\deqn{f(x) = \frac{\Gamma ((n+1)/2)}{\sqrt{\pi n} \Gamma (n/2)}
(1 + x^2/n)^{-(n+1)/2}}{f(x) = Gamma((n+1)/2) / (sqrt(n pi) Gamma(n/2))
            (1 + x^2/n)^-((n+1)/2)}
for all real \eqn{x}.
}
\seealso{
\code{\link{dF}} for the F distribution which generalizes the T one.
}
\examples{
1 - pt(1:5, df = 1)
qt(.975, df = c(1:10,20,50,100,1000))
}
\keyword{distribution}
