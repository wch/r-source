% File src/library/stats/man/cor.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{cor}
\title{Correlation, Variance and Covariance (Matrices)}
\usage{
var(x, y = NULL, na.rm = FALSE, use)

cov(x, y = NULL, use = "everything",
    method = c("pearson", "kendall", "spearman"))

cor(x, y = NULL, use = "everything",
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
  are vectors.   If \code{x} and \code{y} are matrices then the
  covariances (or correlations) between the columns of \code{x} and the
  columns of \code{y} are computed.

  \code{cov2cor} scales a covariance matrix into the corresponding
  correlation matrix \emph{efficiently}.
}
\arguments{
  \item{x}{a numeric vector, matrix or data frame.}
  \item{y}{\code{NULL} (default) or a vector, matrix or data frame with
    compatible dimensions to \code{x}.   The default is equivalent to
    \code{y = x} (but more efficient).}
  \item{na.rm}{logical. Should missing values be removed?}
  \item{use}{an optional character string giving a
    method for computing covariances in the presence
    of missing values.  This must be (an abbreviation of) one of the strings
    \code{"everything"}, \code{"all.obs"}, \code{"complete.obs"},
    \code{"na.or.complete"}, or \code{"pairwise.complete.obs"}.}
  \item{method}{a character string indicating which correlation
    coefficient (or covariance) is to be computed.  One of
    \code{"pearson"} (default), \code{"kendall"}, or \code{"spearman"}:
    can be abbreviated.}
  \item{V}{symmetric numeric matrix, usually positive definite such as a
    covariance matrix.}
}
\value{For \code{r <- cor(*, use = "all.obs")}, it is now guaranteed that
  \code{all(abs(r) <= 1)}.
}
\details{
  For \code{cov} and \code{cor} one must \emph{either} give a matrix or
  data frame for \code{x} \emph{or} give both \code{x} and \code{y}.

  The inputs must be numeric (as determined by \code{\link{is.numeric}}:
  logical values are also allowed for historical compatibility): the
  \code{"kendall"} and \code{"spearman"} methods make sense for ordered
  inputs but \code{\link{xtfrm}} can be used to find a suitable prior
  transformation to numbers.

  \code{var} is just another interface to \code{cov}, where
  \code{na.rm} is used to determine the default for \code{use} when that
  is unspecified.  If \code{na.rm} is \code{TRUE} then the complete
  observations (rows) are used (\code{use = "na.or.complete"}) to
  compute the variance.  Otherwise, by default \code{use = "everything"}.

  If \code{use} is \code{"everything"}, \code{\link{NA}}s will
  propagate conceptually, i.e., a resulting value will be \code{NA}
  whenever one of its contributing observations is \code{NA}.\cr
  If \code{use} is \code{"all.obs"}, then the presence of missing
  observations will produce an error.  If \code{use} is
  \code{"complete.obs"} then missing values are handled by casewise
  deletion (and if there are no complete cases, that gives an error).
  \cr
  \code{"na.or.complete"} is the same unless there are no complete
  cases, that gives \code{NA}.
  Finally, if \code{use} has the value \code{"pairwise.complete.obs"}
  then the correlation or covariance between each pair of variables is
  computed using all complete pairs of observations on those variables.
  This can result in covariance or correlation matrices which are not positive
  semi-definite, as well as \code{NA} entries if there are no complete
  pairs for that pair of variables.   For \code{cov} and \code{var},
  \code{"pairwise.complete.obs"} only works with the \code{"pearson"}
  method.
  Note that (the equivalent of) \code{var(double(0), use = *)} gives
  \code{NA} for \code{use = "everything"} and \code{"na.or.complete"},
  and gives an error in the other cases.

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
  computes \code{cor(R(x), R(y))} (or \code{cov(., .)}) where \code{R(u)
  := rank(u, na.last = "keep")}. In the case of missing values, the
  ranks are calculated depending on the value of \code{use}, either
  based on complete observations, or based on pairwise completeness with
  reranking for each pair.

  When there are ties, Kendall's \eqn{\tau_b}{tau_b} is computed, as
  proposed by Kendall (1945).

  Scaling a covariance matrix into a correlation one can be achieved in
  many ways, mathematically most appealing by multiplication with a
  diagonal matrix from left and right, or more efficiently by using
  \code{\link{sweep}(.., FUN = "/")} twice.  The \code{cov2cor} function
  is even a bit more efficient, and provided mostly for didactical
  reasons.
}
\note{
  Some people have noted that the code for Kendall's tau is slow for
  very large datasets (many more than 1000 cases).  It rarely makes
  sense to do such a computation, but see function
  \code{\link[pcaPP:cor.fk]{cor.fk}} in package \CRANpkg{pcaPP}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Kendall, M. G. (1938)  A new measure of rank correlation,
  \emph{Biometrika} \bold{30}, 81--93. \url{https://dx.doi.org/10.1093/biomet/30.1-2.81}

  Kendall, M. G. (1945)  The treatment of ties in rank problems.
  \emph{Biometrika} \bold{33} 239--251. \url{https://dx.doi.org/10.1093/biomet/33.3.239}
}
\seealso{
  \code{\link{cor.test}} for confidence intervals (and tests).

  \code{\link{cov.wt}} for \emph{weighted} covariance computation.

  \code{\link{sd}} for standard deviation (vectors).
}
\examples{
var(1:10)  # 9.166667

var(1:5, 1:5) # 2.5

## Two simple vectors
cor(1:10, 2:11) # == 1

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
##           to become the correlation matrix.
cov2cor # see the function definition {and learn ..}
stopifnot(all.equal(Cl, cov2cor(cov(longley))),
          all.equal(cor(longley, method = "kendall"),
            cov2cor(cov(longley, method = "kendall"))))

##--- Missing value treatment:
% "everything", "all.obs", "complete.obs", "na.or.complete", "pairwise.complete.obs"
C1 <- cov(swiss)
range(eigen(C1, only.values = TRUE)$values) # 6.19        1921

## swM := "swiss" with  3 "missing"s :
swM <- swiss
colnames(swM) <- abbreviate(colnames(swiss), min=6)
swM[1,2] <- swM[7,3] <- swM[25,5] <- NA # create 3 "missing"

## Consider all 5 "use" cases :
(C. <- cov(swM)) # use="everything"  quite a few NA's in cov.matrix
try(cov(swM, use = "all")) # Error: missing obs...
C2 <- cov(swM, use = "complete")
stopifnot(identical(C2, cov(swM, use = "na.or.complete")))
range(eigen(C2, only.values = TRUE)$values) # 6.46   1930
C3 <- cov(swM, use = "pairwise")
range(eigen(C3, only.values = TRUE)$values) # 6.19   1938

## Kendall's tau doesn't change much:
symnum(Rc <- cor(swM, method = "kendall", use = "complete"))
symnum(Rp <- cor(swM, method = "kendall", use = "pairwise"))
symnum(R. <- cor(swiss, method = "kendall"))

## "pairwise" is closer componentwise,
summary(abs(c(1 - Rp/R.)))
summary(abs(c(1 - Rc/R.)))

## but "complete" is closer in Eigen space:
EV <- function(m) eigen(m, only.values=TRUE)$values
summary(abs(1 - EV(Rp)/EV(R.)) / abs(1 - EV(Rc)/EV(R.)))
}
\keyword{univar}
\keyword{multivariate}
\keyword{array}
