\name{F}
\title{The F Distribution}
\usage{
df(x, n1, n2)
pf(q, n1, n2)
qf(p, n1, n2)
rf(n, n1, n2)
}
\alias{df}
\alias{pf}
\alias{qf}
\alias{rf}
\value{
  These functions provide information about the F distribution with
  \code{n1} and \code{n2} degrees of freedom. 
  \code{df} gives the density,
  \code{pf} gives the distribution function
  \code{qf} gives the quantile function
  and
  \code{rf} generates random deviates.

  The F distribution with \eqn{n_1}{n1} and \eqn{n_2}{n2} degrees of
  freedom has density
  \deqn{
    f(x) =
    \frac{\Gamma(n_1/2 + n_2/2)}{\Gamma(n_1/2)\Gamma(n_2/2)}
    \left(\frac{n_1}{n_2}\right)^{n_1/2}
    x^{n_1/2-1}
    \left(1 + \frac{n_1 x}{n_2}\right)^{-(n_1 + n_2) / 2}}{
    f(x) =
    Gamma((n1 + n2)/2) / (Gamma(n1/2) Gamma(n2/2))
    (n1/n2)^(n1/2) x^(n1/2 - 1)
    (1 + (n1/n2) x)^-(n1 + n2)/2}
  for \eqn{x > 0}.
}
\seealso{
  \code{\link{dt}} for Student's t distribution which is (almost)
  equivalent to \code{*f(\dots, n2 = 1)}.
}
\examples{
df(1,1,1) == dt(1,1)
}
\keyword{distribution}
