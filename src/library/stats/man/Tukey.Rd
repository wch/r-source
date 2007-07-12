\name{Tukey}
\alias{Tukey}
\title{The Studentized Range Distribution}
\description{
  Functions of the distribution of the studentized range, \eqn{R/s},
  where \eqn{R} is the range of a standard normal sample and
  \eqn{df \times s^2}{df*s^2} is independently distributed as
  chi-squared with \eqn{df} degrees of freedom, see \code{\link{pchisq}}.
}
\usage{
ptukey(q, nmeans, df, nranges = 1, lower.tail = TRUE, log.p = FALSE)
qtukey(p, nmeans, df, nranges = 1, lower.tail = TRUE, log.p = FALSE)
}
\alias{ptukey}
\alias{qtukey}
\arguments{
  \item{q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{nmeans}{sample size for range (same for each group).}
  \item{df}{degrees of freedom for \eqn{s} (see below).}
  \item{nranges}{number of \emph{groups} whose \bold{maximum} range is
    considered.}
  \item{log.p}{logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]}{P[X <= x]}, otherwise, \eqn{P[X > x]}{P[X > x]}.}
}
\details{
  If \eqn{n_g =}{ng =}\code{nranges} is greater than one, \eqn{R} is
  the \emph{maximum} of \eqn{n_g}{ng} groups of \code{nmeans}
  observations each.
}
\value{
  \code{ptukey} gives the distribution function and \code{qtukey} its
  inverse, the quantile function.
}
\note{
  A Legendre 16-point formula is used for the integral of \code{ptukey}.
  The computations are relatively expensive, especially for
  \code{qtukey} which uses a simple secant method for finding the
  inverse of \code{ptukey}.
  \code{qtukey} will be accurate to the 4th decimal place.
}
\references{
  Copenhaver, Margaret Diponzio and Holland, Burt S. (1988)
  Multiple comparisons of simple effects in
  the two-way analysis of variance with fixed effects.
  \emph{Journal of Statistical Computation and Simulation}, \bold{30}, 1--15.
}
\seealso{
  \code{\link{pnorm}} and \code{\link{qnorm}} for the corresponding
  functions for the normal distribution.
}
\examples{
if(interactive())
  curve(ptukey(x, nm=6, df=5), from=-1, to=8, n=101)
(ptt <- ptukey(0:10, 2, df= 5))
(qtt <- qtukey(.95, 2, df= 2:11))
## The precision may be not much more than about 8 digits:
summary(abs(.95 - ptukey(qtt,2, df = 2:11)))
}
\keyword{distribution}
