% File src/library/base/man/qraux.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Copyright 2002-2015 The R Foundation
% Distributed under GPL 2 or later

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

  Note that \code{qr.Q(qr, *)} is a special case of
  \code{\link{qr.qy}(qr, y)} (with a \dQuote{diagonal} \code{y}), and
  \code{qr.X(qr, *)} is basically \code{\link{qr.qy}(qr, R)} (apart from
  pivoting and \code{dimnames} setting).

 \code{qr.R} returns \bold{R}.  This may be pivoted, e.g., if
 \code{a <- qr(x)} then \code{x[, a$pivot]} = \bold{QR}.  The number of
 rows of \bold{R} is either \code{nrow(X)} or \code{ncol(X)} (and may
 depend on whether \code{complete} is \code{TRUE} or \code{FALSE}).
}
\seealso{
  \code{\link{qr}},
  \code{\link{qr.qy}}.
}
\examples{
p <- ncol(x <- LifeCycleSavings[, -1]) # not the 'sr'
qrstr <- qr(x)   # dim(x) == c(n,p)
qrstr $ rank # = 4 = p
Q <- qr.Q(qrstr) # dim(Q) == dim(x)
R <- qr.R(qrstr) # dim(R) == ncol(x)
X <- qr.X(qrstr) # X == x
range(X - as.matrix(x))  # ~ < 6e-12
## X == Q \%*\% R if there has been no pivoting, as here:
all.equal(unname(X),
          unname(Q \%*\% R))

# example of pivoting
x <- cbind(int = 1,
           b1 = rep(1:0, each = 3), b2 = rep(0:1, each = 3),
           c1 = rep(c(1,0,0), 2), c2 = rep(c(0,1,0), 2), c3 = rep(c(0,0,1),2))
x # is singular, columns "b2" and "c3" are "extra"
a <- qr(x)
zapsmall(qr.R(a)) # columns are int b1 c1 c2 b2 c3
a$pivot
pivI <- sort.list(a$pivot) # the inverse permutation
all.equal (x,            qr.Q(a) \%*\% qr.R(a)) # no, no
stopifnot(
 all.equal(x[, a$pivot], qr.Q(a) \%*\% qr.R(a)),          # TRUE
 all.equal(x           , qr.Q(a) \%*\% qr.R(a)[, pivI]))  # TRUE too!
}
\keyword{algebra}
\keyword{array}
