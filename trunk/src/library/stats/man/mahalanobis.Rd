\name{mahalanobis}
\title{Mahalanobis Distance}
\usage{
mahalanobis(x, center, cov, inverted=FALSE, ...)
}
\alias{mahalanobis}
\arguments{
  \item{x}{vector or matrix of data with, say, \eqn{p} columns.}
  \item{center}{mean vector of the distribution or second data vector of
    length \eqn{p}.}
  \item{cov}{covariance matrix (\eqn{p \times p}{p x p}) of the distribution.}
  \item{inverted}{logical.  If \code{TRUE}, \code{cov} is supposed to
    contain the \emph{inverse} of the covariance matrix.}
  \item{...}{passed to \code{\link{solve}} for computing the inverse of
    the covariance matrix (if \code{inverted} is false).}
}
\description{
  Returns the squared Mahalanobis distance of all rows in \code{x} and the
  vector \eqn{\mu=}\code{center} with respect to
  \eqn{\Sigma=}\code{cov}.
  This is (for vector \code{x}) defined as
  \deqn{
    D^2 = (x - \mu)' \Sigma^{-1} (x - \mu)
  }
}
\seealso{\code{\link{cov}}, \code{\link{var}}}
\examples{
ma <- cbind(1:6, 1:3)
(S <-  var(ma))
mahalanobis(c(0,0), 1:2, S)

x <- matrix(rnorm(100*3), ncol = 3)
stopifnot(mahalanobis(x, 0, diag(ncol(x))) == rowSums(x*x))
	##- Here, D^2 = usual squared Euclidean distances

Sx <- cov(x)
D2 <- mahalanobis(x, colMeans(x), Sx)
plot(density(D2, bw=.5),
     main="Squared Mahalanobis distances, n=100, p=3") ; rug(D2)
qqplot(qchisq(ppoints(100), df=3), D2,
       main = expression("Q-Q plot of Mahalanobis" * ~D^2 *
                         " vs. quantiles of" * ~ chi[3]^2))
abline(0, 1, col = 'gray')
}
\keyword{multivariate}
