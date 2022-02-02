% File src/library/stats/man/cov.wt.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{cov.wt}
\alias{cov.wt}
\title{Weighted Covariance Matrices}
\usage{
cov.wt(x, wt = rep(1/nrow(x), nrow(x)), cor = FALSE, center = TRUE,
       method = c("unbiased", "ML"))
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
  \item{cor}{a logical indicating whether the estimated correlation
    weighted matrix will be returned as well.}
  \item{center}{either a logical or a numeric vector specifying the
    centers to be used when computing covariances.  If \code{TRUE}, the
    (weighted) mean of each variable is used, if \code{FALSE}, zero is
    used.  If \code{center} is numeric, its length must equal the number
    of columns of \code{x}.}
  \item{method}{string specifying how the result is scaled, see
    \sQuote{Details} below.  Can be abbreviated.}
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
  By default, \code{method = "unbiased"},
  The covariance matrix is divided by one minus the sum of squares of
  the weights, so if the weights are the default (\eqn{1/n}) the conventional
  unbiased estimate of the covariance matrix with divisor \eqn{(n - 1)}
  is obtained.  This differs from the behaviour in S-PLUS which
  corresponds to \code{method = "ML"} and does not divide.
}
\seealso{\code{\link{cov}} and \code{\link{var}}.
}
\examples{
 (xy <- cbind(x = 1:10, y = c(1:3, 8:5, 8:10)))
 w1 <- c(0,0,0,1,1,1,1,1,0,0)
 cov.wt(xy, wt = w1) # i.e. method = "unbiased"
 cov.wt(xy, wt = w1, method = "ML", cor = TRUE)
}
\keyword{multivariate}
