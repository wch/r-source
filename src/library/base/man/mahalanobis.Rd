\name{mahalanobis}
\title{Mahalanobis Distance}
\usage{
mahalanobis(x, center, cov, inverted=FALSE)
}
\alias{mahalanobis}
\arguments{
 \item{x}{Vector or matrix of data.}
 \item{center}{Mean vector of the distribution or second data vector.}
 \item{cov}{Covariance matrix of the distribution or the QR
   decomposition of the distribution.}
 \item{inverted}{If \code{TRUE}, \code{cov} is supposed to contain the
   inverse of the covariance matrix.}
}
\description{
  Returns the Mahalanobis distance of all rows in \code{x} and the
  vector \code{center}, which is (for vector \code{x}) defined as
  \deqn{
    d = (x - center)' cov^{-1} (x - center)
  }
}
\author{Friedrich Leisch}
\seealso{\code{\link{qr}},\code{\link{cov}}}
\examples{
x <- matrix(rnorm(30),ncol=3)
mahalanobis(x)
}
\keyword{multivariate}
