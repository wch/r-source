\name{cov.wt}
\alias{cov.wt}
\title{Weighted Covariance Matrices}
\usage{
cov.wt(x, wt = rep(1/nrow(x), nrow(x)), cor = FALSE, center = TRUE)
}
\description{
  Returns a list containing estimates of the weighted covariance matrix
  and the mean of the data, and optionally of the (weighted) correlation
  matrix.}
\arguments{
  \item{x}{a matrix or data frame.  As usual, rows are observations and
    columns are variables.}
  \item{wt}{a non-negative and non-zero vector of weights for each
    observation.  Its length must equal the number of rows of \code{x}.}
  \item{cor}{A logical indicating whether the estimated correlation
    weighted matrix will be returned as well.}
  \item{center}{Either a logical or a numeric vector specifying the
    centers to be used when computing covariances.  If \code{TRUE}, the
    (weighted) mean of each variable is used, if \code{FALSE}, zero is
    used.  If \code{center} is numeric, its length must equal the number
    of columns of \code{x}.}
}
\value{
  A list containing the following named components:
  \item{cov}{the estimated (weighted) covariance matrix}
  \item{center}{an estimate for the center (mean) of the data.}
  \item{n.obs}{the number of observations (rows) in \code{x}.}
  \item{wt}{the weights used in the estimation.  Only returned if given
    as an argument.}
  \item{cor}{the estimated correlation matrix.  Only returned if
    \code{cor} is \code{TRUE}.}
}
\details{
  The covariance matrix is divided by one minus the sum of squares of
  the weights, so if the weights are the default (\eqn{1/n}) the conventional
  unbiased estimate of the covariance matrix with divisor \eqn{(n - 1)}
  is obtained.  This differs from the behaviour in S-PLUS.
}
\seealso{\code{\link{cov}} and \code{\link{var}}.
}
\keyword{multivariate}
