\name{backsolve}
\title{Solve an Upper Triangular System}
\usage{
backsolve(r, x, k=ncol(r))
}
\alias{backsolve}
\arguments{
\item{r}{an upper triangular matrix giving the coefficients for
the system to be solved.  Values below the diagonal are ignored.}
\item{x}{a matrix whose columns give ``right-hand sides'' for the equations.}
\item{k}{The number or columns of \code{r} and rows of \code{x} to use.}
}
\value{
The solution of the triangular system.  The result will be a
a vector if \code{x} is a vector and a matrix if \code{x} is a matrix.
}
\references{
Dongarra, J. J., J. R. Bunch, C. B. Moler and G. W. Stewart (1978).
\emph{LINPACK Users Guide}.  Philadelphia: SIAM Publications.
}
\seealso{
\code{\link{chol}},
\code{\link{qr}},
\code{\link{solve}}.
}
\examples{
# no examples yet
}
\keyword{algebra}
\keyword{array}
