\name{cor}
\title{Correlation, Variance and Covariance (Matrices)}
\usage{
var(x, y = NULL, na.rm = FALSE, use)
cor(x, y = NULL, use = "all.obs")
cov(x, y = NULL, use = "all.obs")
}
\alias{var}
\alias{cor}
\alias{cov}
\description{
  \code{var}, \code{cov} and \code{cor} compute the variance of \code{x}
  and the covariance or correlation of \code{x} and \code{y} if these
  are vectors.  If \code{x} and \code{y} are matrices then the
  covariances (or correlations) between the columns of \code{x} and the
  columns of \code{y} are computed.
}
\arguments{
  \item{x}{a numeric vector, matrix or data frame.}
  \item{y}{\code{NULL} (default) or a vector, matrix or data frame with
    compatible dimensions to \code{x}.  The default is equivalent to
    \code{y = x} (but more efficient).}
  \item{use}{an optional character string giving a
    method for computing covariances in the presence
    of missing values.  This must be (an abbreviation of) one of the strings
    \code{"all.obs"}, \code{"complete.obs"}
    or \code{"pairwise.complete.obs"}.}
  \item{na.rm}{logical. Should missing values be removed?}
}
\value{For \code{r <- cor(*, use = "all.obs")}, it is now guaranteed that
  \code{all(r <= 1)}.
}
\details{
  For \code{cov} and \code{cor} one must \emph{either} give a matrix or data
  frame for \code{x} \emph{or} give both \code{x} and \code{y}.

  \code{var} just another interface to \code{cov}, where
  \code{na.rm} is used to determine the default for \code{use} when that
  is unspecified.  If \code{na.rm} is \code{TRUE} then the complete
  observations (rows) are used (\code{use = "complete"}) to compute the
  variance.  Otherwise (\code{use = "all"}), \code{var} will give an
  error if there are missing values.

  If \code{use} is \code{"all.obs"}, then the presence
  of missing observations will produce an error.
  If \code{use} is \code{"complete.obs"} then missing values
  are handled by casewise deletion.  Finally, if \code{use} has the
  value \code{"pairwise.complete.obs"} then the correlation between
  each pair of variables is computed using all complete pairs
  of observations on those variables.
  This can result in covariance or correlation matrices which are not
  positive semidefinite.

  The denominator \eqn{n - 1} is used which gives an unbiased estimator
  of the (co)variance for i.i.d. observations.
  These functions return \code{\link{NA}} when there is only one
  observation (whereas S-plus has been returning \code{NaN}),
  and from \R 1.2.3 fail if \code{x} has length zero.
}
\seealso{
  \code{\link[ctest]{cor.test}} (pkg \code{ctest}) for confidence
  intervals (and tests).\cr
  \code{\link{cov.wt}} for \emph{weighted} covariance
  computation, \code{\link{sd}} for standard deviation (vectors).
}
\examples{
var(1:10)# 9.166667

var(1:5,1:5)# 2.5

## Two simple vectors
cor(1:10,2:11)# == 1

## Correlation Matrix of Multivariate sample:
data(longley)
(Cl <- cor(longley))
## Graphical Correlation Matrix:
symnum(Cl) # highly correlated

##--- Missing value treatment:
data(swiss)
C1 <- cov(swiss)
range(eigen(C1, only=TRUE)$val) # 6.19  1921
swiss[1,2] <- swiss[7,3] <- swiss[25,5] <- NA # create 3 "missing"
try(cov(swiss)) # Error: missing obs...
C2 <- cov(swiss, use = "complete")
range(eigen(C2, only=TRUE)$val) # 6.46  1930
C3 <- cov(swiss, use = "pairwise")
range(eigen(C3, only=TRUE)$val) # 6.19  1938
}
\keyword{univar}
\keyword{multivariate}
\keyword{array}
