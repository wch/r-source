\name{Tukey}
\title{The Studentized Range Distribution}
\usage{
ptukey(q, n, df)
qtukey(p, n, df)
}
\alias{ptukey}
\alias{qtukey}
\arguments{
\item{q}{vector of quantiles.}
\item{p}{vector of probabilities.}
\item{n}{sample size for range.}
\item{df}{degrees of freedom.}
}
\value{
    These functions provide information about the distribution of
    the studentized range, \eqn{R/s}, where R is the range of a
    standard normal sample of size \eqn{n} and \eqn{s^2} is independently
    distributed as chi-squared with \eqn{df} degrees of freedom.
    
    \code{ptukey} gives the distribution function and \code{qtukey} gives
    the quantile function.
}
\references{
Copenhaver, Margaret Diponzio & Holland, Burt S.
Multiple comparisons of simple effects in
the two-way analysis of variance with fixed effects.
\emph{Journal of Statistical Computation and Simulation},
Vol.30, pp.1-15, 1988.
}
\seealso{
\code{\link{pnorm}} and \code{\link{qnorm}} for the corresponding
functions for the normal distribution.
}
\examples{
qtukey(.95, 2, df=2:5)
}
\keyword{distribution}
