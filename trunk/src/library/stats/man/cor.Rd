\name{cor}
\title{Correlation, Variance and Covariance (Matrices)}
\usage{
var(x, y = NULL, na.rm = FALSE, use)

cov(x, y = NULL, use = "all.obs",
    method = c("pearson", "kendall", "spearman"))

cor(x, y = NULL, use = "all.obs",
     method = c("pearson", "kendall", "spearman"))

cov2cor(V)
}
\alias{var}
\alias{cov}
\alias{cor}
\alias{cov2cor}
\description{
  \code{var}, \code{cov} and \code{cor} compute the variance of \code{x}
  and the covariance or correlation of \code{x} and \code{y} if these
  are vectors.	If \code{x} and \code{y} are matrices then the
  covariances (or correlations) between the columns of \code{x} and the
  columns of \code{y} are computed.

  \code{cov2cor} scales a covariance matrix into the corresponding
  correlation matrix \emph{efficiently}.
}
\arguments{
  \item{x}{a numeric vector, matrix or data frame.}
  \item{y}{\code{NULL} (default) or a vector, matrix or data frame with
    compatible dimensions to \code{x}.	The default is equivalent to
    \code{y = x} (but more efficient).}
  \item{na.rm}{logical. Should missing values be removed?}
  \item{use}{an optional character string giving a
    method for computing covariances in the presence
    of missing values.	This must be (an abbreviation of) one of the strings
    \code{"all.obs"}, \code{"complete.obs"} or \code{"pairwise.complete.obs"}.}
  \item{method}{a character string indicating which correlation
    coefficient (or covariance) is to be computed.  One of
    \code{"pearson"} (default), \code{"kendall"}, or \code{"spearman"},
    can be abbreviated.}
  \item{V}{symmetric numeric matrix, usually positive definite such as a
    covariance matrix.}
}
\value{For \code{r <- cor(*, use = "all.obs")}, it is now guaranteed that
  \code{all(r <= 1)}.
}
\details{
  For \code{cov} and \code{cor} one must \emph{either} give a matrix or
  data frame for \code{x} \emph{or} give both \code{x} and \code{y}.

  \code{var} is just another interface to \code{cov}, where
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
  positive semi-definite. \code{"pairwise.complete.obs"} only works with
  the \code{"pearson"} method for \code{cov} and \code{var}.


  The denominator \eqn{n - 1} is used which gives an unbiased estimator
  of the (co)variance for i.i.d. observations.
  These functions return \code{\link{NA}} when there is only one
  observation (whereas S-PLUS has been returning \code{NaN}), and 
  fail if \code{x} has length zero.

  For \code{cor()}, if \code{method} is \code{"kendall"} or
  \code{"spearman"}, Kendall's \eqn{\tau}{tau} or Spearman's
  \eqn{\rho}{rho} statistic is used to estimate a rank-based measure of
  association.  These are more robust and have been recommended if the
  data do not necessarily come from a bivariate normal distribution.\cr
  For \code{cov()}, a non-Pearson method is unusual but available for
  the sake of completeness.  Note that \code{"spearman"} basically
  computes \code{cor(R(x), R(y))} (or \code{cov(.,.)}) where
  \code{R(u) := rank(u, na.last="keep")}. In the case of missing values, the
  ranks are calculated depending on the value of \code{use}, either
  based on complete observations, or based on pairwise completeness with
  reranking for each pair.

  Scaling a covariance matrix into a correlation one can be achieved in
  many ways, mathematically most appealing by multiplication with a
  diagonal matrix from left and right, or more efficiently by using
  \code{\link{sweep}(.., FUN = "/")} twice.  The \code{cov2cor} function
  is even a bit more efficient, and provided mostly for didactical
  reasons.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{cor.test}} for confidence intervals (and tests).
  
  \code{\link{cov.wt}} for \emph{weighted} covariance computation.

  \code{\link{sd}} for standard deviation (vectors).
}
\examples{
var(1:10)# 9.166667

var(1:5,1:5)# 2.5

## Two simple vectors
cor(1:10,2:11)# == 1

## Correlation Matrix of Multivariate sample:
(Cl <- cor(longley))
## Graphical Correlation Matrix:
symnum(Cl) # highly correlated

## Spearman's rho  and  Kendall's tau
symnum(clS <- cor(longley, method = "spearman"))
symnum(clK <- cor(longley, method = "kendall"))
## How much do they differ?
i <- lower.tri(Cl)
cor(cbind(P = Cl[i], S = clS[i], K = clK[i]))


## cov2cor() scales a covariance matrix by its diagonal
##	     to become the correlation matrix.
cov2cor # see the function definition {and learn ..}
stopifnot(all.equal(Cl, cov2cor(cov(longley))),
          all.equal(cor(longley, method="kendall"),
            cov2cor(cov(longley, method="kendall"))))

##--- Missing value treatment:
C1 <- cov(swiss)
range(eigen(C1, only=TRUE)$val) # 6.19	1921
swM <- swiss
swM[1,2] <- swM[7,3] <- swM[25,5] <- NA # create 3 "missing"
try(cov(swM)) # Error: missing obs...
C2 <- cov(swM, use = "complete")
range(eigen(C2, only=TRUE)$val) # 6.46	1930
C3 <- cov(swM, use = "pairwise")
range(eigen(C3, only=TRUE)$val) # 6.19	1938

symnum(cor(swM, method = "kendall", use = "complete"))
## Kendall's tau doesn't change much:
symnum(cor(swiss, method = "kendall"))
}
\keyword{univar}
\keyword{multivariate}
\keyword{array}
