% File src/library/stats/man/rWishart.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2012-2014 R Core Team
% Distributed under GPL 2 or later

\name{rWishart}
\alias{rWishart}
\title{Random Wishart Distributed Matrices}
\description{
  Generate \code{n} random matrices, distributed according to the
  Wishart distribution with parameters \code{Sigma} and \code{df},
  \eqn{W_p(\Sigma, m),\ m=\code{df},\ \Sigma=\code{Sigma}}{W_p(Sigma, df)}.
}
\usage{
rWishart(n, df, Sigma)
}
\arguments{
  \item{n}{integer sample size.}
  \item{df}{numeric parameter, \dQuote{degrees of freedom}.}
  \item{Sigma}{positive definite (\eqn{p\times p}{p * p}) \dQuote{scale}
    matrix, the matrix parameter of the distribution.}
}
\details{
  If \eqn{X_1,\dots, X_m, \ X_i\in\mathbf{R}^p}{X1,...,Xm, Xi in R^p} is
  a sample of \eqn{m} independent multivariate Gaussians with mean (vector) 0, and
  covariance matrix \eqn{\Sigma}, the distribution of
  \eqn{M = X'X} is \eqn{W_p(\Sigma, m)}.

  Consequently, the expectation of \eqn{M} is
  \deqn{E[M] = m\times\Sigma.}{E[M] = m * Sigma.}
  Further, if \code{Sigma} is scalar (\eqn{p = 1}), the Wishart
  distribution is a scaled chi-squared (\eqn{\chi^2}{chi^2})
  distribution with \code{df} degrees of freedom,
  \eqn{W_1(\sigma^2, m) = \sigma^2 \chi^2_m}{W_1(sigma^2, m) = sigma^2 chi[m]^2}.

  The component wise variance is
  \deqn{\mathrm{Var}(M_{ij}) = m(\Sigma_{ij}^2 + \Sigma_{ii} \Sigma_{jj}).}{%
    Var(M[i,j]) = m*(S[i,j]^2 + S[i,i] * S[j,j]), where S=Sigma.}
}
\value{
  a numeric \code{\link{array}}, say \code{R}, of dimension
  \eqn{p \times p \times n}{p * p * n}, where each \code{R[,,i]} is a
  positive definite matrix, a realization of the Wishart distribution
  \eqn{W_p(\Sigma, m),\ \ m=\code{df},\ \Sigma=\code{Sigma}}{W_p(Sigma, df)}.
}
\references{
  Mardia, K. V., J. T. Kent, and J. M. Bibby (1979)
  \emph{Multivariate Analysis}, London: Academic Press.
}
\author{Douglas Bates}
\seealso{
  \code{\link{cov}}, \code{\link{rnorm}}, \code{\link{rchisq}}.
}
\examples{
## Artificial
S <- toeplitz((10:1)/10)
set.seed(11)
R <- rWishart(1000, 20, S)
dim(R)  #  10 10  1000
mR <- apply(R, 1:2, mean)  # ~= E[ Wish(S, 20) ] = 20 * S
stopifnot(all.equal(mR, 20*S, tolerance = .009))

## See Details, the variance is
Va <- 20*(S^2 + tcrossprod(diag(S)))
vR <- apply(R, 1:2, var)
stopifnot(all.equal(vR, Va, tolerance = 1/16))
}
\keyword{multivariate}
