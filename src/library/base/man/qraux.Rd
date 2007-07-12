\name{QR.Auxiliaries}
\title{Reconstruct the Q, R, or X Matrices from a QR Object}
\usage{
qr.X(qr, complete = FALSE, ncol =)
qr.Q(qr, complete = FALSE, Dvec =)
qr.R(qr, complete = FALSE)
}
\alias{qr.X}
\alias{qr.Q}
\alias{qr.R}
\arguments{
  \item{qr}{object representing a QR decomposition.  This will
    typically have come from a previous call to \code{\link{qr}} or
    \code{\link{lsfit}}.}
  \item{complete}{logical expression of length 1.  Indicates whether an
    arbitrary  orthogonal completion of the \eqn{\bold{Q}} or
    \eqn{\bold{X}} matrices is to be made, or whether the \eqn{\bold{R}}
    matrix is to be completed  by binding zero-value rows beneath the
    square upper triangle.}
  \item{ncol}{integer in the range \code{1:nrow(qr$qr)}.  The number
    of columns to be in the reconstructed \eqn{\bold{X}}.  The default
    when \code{complete} is \code{FALSE} is the first
    \code{min(ncol(X), nrow(X))} columns of the original \eqn{\bold{X}}
    from which the qr object was constructed.  The default when
    \code{complete} is \code{TRUE} is a square matrix with the original
    \eqn{\bold{X}} in the first \code{ncol(X)} columns and an arbitrary
    orthogonal completion (unitary completion in the complex case) in
    the remaining columns.}
  \item{Dvec}{vector (not matrix) of diagonal values.  Each column of
    the returned \eqn{\bold{Q}} will be multiplied by the corresponding
    diagonal value.  Defaults to all \code{1}s.}
}
\description{
  Returns the original matrix from which the object was constructed or
  the components of the decomposition.
}
\value{
  \code{qr.X} returns \eqn{\bold{X}}, the original matrix from
  which the qr object was constructed, provided \code{ncol(X) <= nrow(X)}.
  If \code{complete} is \code{TRUE} or the argument \code{ncol} is greater than
  \code{ncol(X)}, additional columns from an arbitrary orthogonal
  (unitary) completion of \code{X} are returned.

  \code{qr.Q} returns part or all of \bold{Q}, the order-nrow(X)
  orthogonal (unitary) transformation represented by \code{qr}.  If
  \code{complete} is \code{TRUE}, \bold{Q} has \code{nrow(X)} columns.
  If \code{complete} is \code{FALSE}, \bold{Q} has \code{ncol(X)}
  columns.  When \code{Dvec} is specified, each column of \bold{Q} is
  multiplied by the corresponding value in \code{Dvec}.

  \code{qr.R} returns \bold{R}.  The number of rows of \bold{R} is
  either \code{nrow(X)} or \code{ncol(X)} (and may depend on whether
  \code{complete} is \code{TRUE} or \code{FALSE}.
}
\seealso{
  \code{\link{qr}},
  \code{\link{qr.qy}}.
}
\examples{
p <- ncol(x <- LifeCycleSavings[,-1]) # not the 'sr'
qrstr <- qr(x)   # dim(x) == c(n,p)
qrstr $ rank # = 4 = p
Q <- qr.Q(qrstr) # dim(Q) == dim(x)
R <- qr.R(qrstr) # dim(R) == ncol(x)
X <- qr.X(qrstr) # X == x
range(X - as.matrix(x))# ~ < 6e-12
## X == Q \%*\% R if there has been no pivoting, as here.
Q \%*\% R
}
\keyword{algebra}
\keyword{array}
