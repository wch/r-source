\name{chol2inv}
\title{Inverse from Choleski Decomposition}
\usage{
chol2inv(x, size=ncol(x))
}
\alias{chol2inv}
\arguments{
\item{x}{a matrix.  The first \code{nc} columns of the
upper triangle contain the Choleski decomposition of the
matrix to be inverted.}
\item{size}{the number of columns of \code{x} containing the
choleski decomposition.}
}
\value{This matrix uses the given Choleski decomposition
to invert the original positive definite matrix.
It returns the inverse of the decomposed matrix.
}
\references{
Dongarra, J. J., J. R. Bunch, C. B. Moler and G. W. Stewart (1978).
\emph{LINPACK Users Guide}.
Philadelphia:
SIAM Publications.
}
\seealso{\code{\link{chol}}, \code{\link{solve}}.}
\examples{
cma <- chol(ma  <- cbind(1, 1:3, c(1,3,7)))
t(cma) \%*\% cma # = ma
all(diag(3) == ma \%*\% chol2inv(cma))# TRUE (at least approx.)
}
\keyword{algebra}
\keyword{array}
