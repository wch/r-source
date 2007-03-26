\name{backsolve}
\alias{backsolve}
\alias{forwardsolve}
\title{Solve an Upper or Lower Triangular System}
\description{
  Solves a system of linear equations where the coefficient matrix is
  upper or lower triangular.
}
\usage{
   backsolve(r, x, k=ncol(r), upper.tri=TRUE, transpose=FALSE)
forwardsolve(l, x, k=ncol(l), upper.tri=FALSE, transpose=FALSE)
}
\arguments{
  % Name 'r' is not really making sense for upper.tri = FALSE
  % Name 'x' is also a  misnomer,  should rather be  'b'. -- is this S ??
  \item{r,l}{an upper (or lower) triangular matrix giving the
    coefficients for the system to be solved.  Values below (above)
    the diagonal are ignored.}
  \item{x}{a matrix whose columns give \dQuote{right-hand sides} for
    the equations.}
  \item{k}{The number of columns of \code{r} and rows of \code{x} to use.}
  \item{upper.tri}{logical; if \code{TRUE} (default), the \emph{upper}
    \emph{tri}angular part of \code{r} is used.  Otherwise, the lower one.}
  \item{transpose}{logical; if \code{TRUE}, solve \eqn{r' * y = x} for
    \eqn{y}, i.e., \code{t(r) \%*\% y == x}.}
}
\value{
  The solution of the triangular system.  The result will be a vector if
  \code{x} is a vector and a matrix if \code{x} is a matrix.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.

  Dongarra, J. J., Bunch,J. R.,  Moler, C. B. and  Stewart, G. W. (1978)
  \emph{LINPACK Users Guide.}  Philadelphia: SIAM Publications.
}
\seealso{
  \code{\link{chol}},
  \code{\link{qr}},
  \code{\link{solve}}.
}
\examples{
## upper triangular matrix 'r':
r <- rbind(c(1,2,3),
           c(0,1,1),
           c(0,0,2))
( y <- backsolve(r, x <- c(8,4,2)) ) # -1 3 1
r \%*\% y # == x = (8,4,2)
backsolve(r, x, transpose = TRUE) # 8 -12 -5
}
\keyword{algebra}
\keyword{array}
