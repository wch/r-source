\name{chol}
\title{The Choleski Decomposition}
\usage{
chol(x)
}
\alias{chol}
\arguments{
\item{x}{a symmetric positive-definite matrix.}
}
\value{
This function computes the Choleski factorization
of its argument.  It returns the upper triangular
factor of the decomposition as its value.
}
\references{
Dongarra, J. J., J. R. Bunch, C. B. Moler and G. W. Stewart (1978).
\emph{LINPACK Users Guide}.  Philadelphia: SIAM Publications.
}
\seealso{
\code{\link{backsolve}},
\code{\link{qr}},
\code{\link{svd}}.
}
\keyword{algebra}
\keyword{array}
