\name{QR.Auxiliaries}
\title{Reconstruct the Q, R, or X Matrices from a QR Object}
\usage{
qr.X(qrstr, complete = FALSE, ncol =)
qr.Q(qrstr, complete = FALSE, Dvec = 1)
qr.R(qrstr, complete = FALSE)
}
\alias{qr.X}
\alias{qr.Q}
\alias{qr.R}
\arguments{
  \item{qrstr}{object representing a QR decomposition.  This will
    typically have come from a previous call to \code{\link{qr}} or
    \code{\link{lsfit}}.}
  \item{complete}{logical expression of length 1.  Indicates whether an
    arbitrary  orthogonal completion of the \eqn{\bold{Q}} or
    \eqn{\bold{X}} matrices is to be made, or whether the \eqn{\bold{R}}
    matrix is to be completed  by binding zero-value rows beneath the
    square upper triangle.}
  \item{ncol}{integer in the range \code{1:nrow(qrstr$qr)}.  The number
    of columns to be in the reconstructed \eqn{\bold{X}}.  The default
    when \code{complete} is \code{FALSE} is the original \eqn{\bold{X}}
    from which the qr object was constructed.  The default when
    \code{complete} is \code{TRUE} is a square matrix with the original
    \eqn{\bold{X}} in the first \code{ncol(X)} columns and an arbitrary
    orthogonal completion (unitary completion in the complex case) in
    the remaining columns.}
  \item{Dvec}{vector (not matrix) of diagonal values.  Each column of
    the returned \eqn{\bold{Q}} will be multiplied by the corresponding
    diagonal value.}
}
\description{
  Returns the original matrix from which the object was constructed or
  the components of the decomposition.
}
\value{\code{qr.X} returns \eqn{\bold{X}}, the original matrix from
  which the qr object was constructed.  If \code{complete} is
  \code{TRUE} or the argument \code{ncol} is greater than
  \code{ncol(X)}, additional columns from an arbitrary orthogonal
  (unitary) completion of \code{X} are returned.

  \code{qr.Q} returns \bold{Q}, the order-nrow(X) orthogonal (unitary)
  transformation represented by qrstr.  If \code{complete} is
  \code{TRUE}, \bold{Q} has \code{nrow(X)} columns.  If \code{complete}
  is \code{FALSE}, \bold{Q} has \code{ncol(X)} columns.  When
  \code{Dvec} is specified, each column of \bold{Q} is multiplied by the
  corresponding value in \code{Dvec}.

  \code{qr.R} returns \bold{R}, the upper triangular matrix such that
  \code{X == Q \%*\% R}.  The number of rows of \bold{R} is
  \code{nrow(X)} or \code{ncol(X)}, depending on whether \code{complete}
  is \code{TRUE} or \code{FALSE}.
}
\seealso{ \code{\link{qr}}, \code{\link{qr.qy}.}
\examples{
data(savings)
p <- ncol(x <- savings[,-1]) # not the 'sr'
qrstr <- qr(x)   # dim(x) == c(n,p)
qrstr $ rank # = 4 = p
Q <- qr.Q(qrstr) # dim(Q) == dim(x)
R <- qr.R(qrstr) # dim(R) == ncol(x)
X <- qr.X(qrstr) # X == x
range(X - as.matrix(x))# ~ < 6e-12

## X == Q \%*\% R :
all((1 - X /( Q \%*\% R))< 100*.Machine$double.eps)#TRUE

dim(Qc <- qr.Q(qrstr, complete=TRUE)) # Square: dim(Qc) == rep(nrow(x),2)
all((crossprod(Qc) - diag(nrow(x))) < 10*.Machine $double.eps)

QD <- qr.Q(qrstr, D=1:p)      # QD == Q \%*\% diag(1:p)
all(QD - Q \%*\% diag(1:p)  < 8* .Machine$double.eps)

dim(Rc <- qr.R(qrstr, complete=TRUE)) # == dim(x)
dim(Xc <- qr.X(qrstr, complete=TRUE)) # square: nrow(x) ^ 2
all(Xc[,1:p] == X)
}
\keyword{algebra}
