\name{cor}
\title{Correlation and Covariance Matrices}
\usage{
cor(x, y=x, use="all.obs")
cov(x, y=x, use="all.obs")
}
\alias{cor}
\alias{cov}
\arguments{
\item{x}{a matrix or data frame.}
\item{y}{a matrix or data frame.}
\item{use}{a character string giving the method for handling
    missing observations. This must be one of the stringss
    \code{"all.obs"}, \code{"complete.obs"} or \code{"pairwise.complete.obs"}
    (abbreviations are acceptable).}
}
\description{
    Compute the correlation or covariance matrix
    of the columns of \code{x} and the columns of \code{y}.
}
\details{
If \code{use} is \code{"all.obs"}, then the presence
of missing observations will cause the computation to fail.
If \code{use} has the value \code{"complete.obs"} then missing values
are handled by casewise deletion.  Finally, if \code{use} has the
value \code{"pairwise.complete.obs"} then the correlation between
each pair of variables is computed using all complete pairs
of observations on those variables.
This can result in covariance or correlation matrices which are not
positive semidefinite.
}
\seealso{\code{\link{cov.wt}} for \emph{weighted} covariance computation.}
\examples{
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
range(eigen(C1, only=T)$val) # 6.19  1921
swiss[1,2] <- swiss[7,3] <- swiss[25,5] <- NA # create 3 "missing"
\dontrun{
 C2 <- cov(swiss) # Error: missing obs...
}
C2 <- cov(swiss, use = "complete")
range(eigen(C2, only=T)$val) # 6.46  1930
C3 <- cov(swiss, use = "pairwise")
range(eigen(C3, only=T)$val) # 6.19  1938
}
\keyword{multivariate}
\keyword{array}
