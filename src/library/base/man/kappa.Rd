% File src/library/base/man/kappa.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2024 R Core Team
% Copyright 2008-2010 The R Foundation
% Distributed under GPL 2 or later

\name{kappa}
\alias{rcond}
\alias{kappa}
\alias{kappa.default}
\alias{kappa.lm}
\alias{kappa.qr}
\alias{.kappa_tri}

\title{Compute or Estimate the Condition Number of a Matrix}
\usage{
kappa(z, \dots)
\method{kappa}{default}(z, exact = FALSE,
      norm = NULL, method = c("qr", "direct"),
      inv_z = solve(z),
      triangular = FALSE, uplo = "U", \dots)

\method{kappa}{lm}(z, \dots)
\method{kappa}{qr}(z, \dots)

.kappa_tri(z, exact = FALSE, LINPACK = TRUE, norm = NULL, uplo = "U", \dots)

rcond(x, norm = c("O","I","1"), triangular = FALSE, uplo = "U", \dots)
}
\arguments{
  \item{z, x}{a numeric or complex matrix or a result of
    \code{\link{qr}} or a fit from a class inheriting from \code{"lm"}.}
  \item{exact}{logical.  Should the result be exact (up to small rounding
    error) as opposed to fast (but quite inaccurate)?}
  \item{norm}{character string, specifying the matrix norm with respect
    to which the condition number is to be computed, see the function
    \code{\link{norm}()}.  For \code{kappa()}, the default is \code{"2"},
    for \code{rcond()} it is \code{"O"}, and for \code{.kappa_tri()}), the
    default depends on \code{exact}: if that is true, the default is
    \code{"2"}, otherwise \code{"O"},
    meaning the \bold{O}ne- or 1-norm.  For \code{exact=FALSE}, the
    currently only other possible value is \code{"I"} for the infinity
    norm.  For \code{exact=TRUE}, norm may be \code{"2"}, or any of the
    possible \code{type} values in \code{\link{norm}(., type = *)}.}
  \item{method}{a partially matched character string specifying the method to be used;
    \code{"qr"} is the default for back-compatibility, mainly.}
  \item{inv_z}{for \code{exact=TRUE, norm != "2"}, (an approximation of)
    \code{\link{solve}(z)}; could be the pseudo inverse or a fast
    approximate inverse of the matrix \code{z}.  By default,
    \code{solve(z)} is the most expensive part of the condition computation
    when \code{exact} is true.}
  \item{triangular}{logical.  If true, the matrix used is just the upper or
    lower triangular part of \code{z} (or \code{x}), depending on}
  \item{uplo}{character string, either \code{"U"} or \code{"L"}.  Used only
    when \code{triangular = TRUE}, indicates if the upper or lower
    triangular part of the matrix is to be used.}
  \item{LINPACK}{logical.  If true and \code{z} is not complex, the
    LINPACK routine \code{dtrco()} is called; otherwise the relevant
    LAPACK routine is.}
  \item{\dots}{further arguments passed to or from other methods;
    for \code{kappa.*()}, notably \code{LINPACK} when \code{norm} is not
    \code{"2"}.}
}
\description{
  The condition number of a regular (square) matrix is the product of
  the \emph{norm} of the matrix and the norm of its inverse (or
  pseudo-inverse), and hence depends on the kind of matrix-norm.

  \code{kappa()} computes by default (an estimate of) the 2-norm
  condition number of a matrix or of the \eqn{R} matrix of a \eqn{QR}
  decomposition, perhaps of a linear fit.  The 2-norm condition number
  can be shown to be the ratio of the largest to the smallest
  \emph{non-zero} singular value of the matrix.

  \code{rcond()} computes an approximation of the \bold{r}eciprocal
  \bold{cond}ition number, see the details.
}
\details{
  For \code{kappa()}, if \code{exact = FALSE} (the default) the
  condition number is estimated by a cheap approximation to the 1-norm of
  the triangular matrix \eqn{R} of the \code{\link{qr}(x)} decomposition
  \eqn{z = QR}.  However, the exact 2-norm calculation (via
  \code{\link{svd}}) is also likely to be quick enough.
  % For hilbert(n), profiling indicates 'exact=TRUE' to be even faster

  Note that the approximate 1- and Inf-norm condition numbers via
  \code{method = "direct"} are much faster to
  calculate, and \code{rcond()} computes these \emph{\bold{r}eciprocal}
  condition numbers, also for complex matrices, using standard LAPACK
  routines.
  Currently, also the \code{kappa*()} functions compute these
  approximations whenever \code{exact} is false, i.e., by default.

  \code{kappa} and \code{rcond} are different interfaces to
  \emph{partly} identical functionality.

  \code{.kappa_tri} is an internal function called by \code{kappa.qr} and
  \code{kappa.default}; \code{tri} is for \emph{tri}angular and its methods
  only consider the upper or lower triangular part of the matrix, depending
  on \code{uplo = "U"} or \code{"L"}, where \code{"U"} was internally hard
  wired before \R 4.4.0.

  Unsuccessful results from the underlying LAPACK code will result in an
  error giving a positive error code: these can only be interpreted by
  detailed study of the FORTRAN code.
}
\value{
  The condition number, \eqn{kappa}, or an approximation if
  \code{exact = FALSE}.
}
\source{
  The LAPACK routines \code{DTRCON} and \code{ZTRCON} and the LINPACK
  routine \code{DTRCO}.

  LAPACK and LINPACK are from \url{https://netlib.org/lapack/} and
  \url{https://netlib.org/linpack/} and their guides are listed
  in the references.
}
\references{
  Anderson. E. and ten others (1999)
  \emph{LAPACK Users' Guide}. Third Edition. SIAM.\cr
  Available on-line at
  \url{https://netlib.org/lapack/lug/lapack_lug.html}.

 Chambers, J. M. (1992)
  \emph{Linear models.}
  Chapter 4 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.

  Dongarra, J. J., Bunch, J. R., Moler, C. B. and Stewart, G. W. (1978)
  \emph{LINPACK Users Guide.}  Philadelphia: SIAM Publications.
}
\author{
  The design was inspired by (but differs considerably from)
  the S function of the same name described in Chambers (1992).
}

\seealso{
  \code{\link{norm}};
  \code{\link{svd}} for the singular value decomposition and
  \code{\link{qr}} for the \eqn{QR} one.
}
\examples{
kappa(x1 <- cbind(1, 1:10)) # 15.71
kappa(x1, exact = TRUE)     # 13.68
kappa(x2 <- cbind(x1, 2:11)) # high! [x2 is singular!]

hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, `+`) }
sv9 <- svd(h9 <- hilbert(9))$ d
kappa(h9)  # pretty high; by default {exact=FALSE, method="qr"} :
kappa(h9) == kappa(qr.R(qr(h9)), norm = "1")
all.equal(kappa(h9, exact = TRUE), # its definition:
          max(sv9) / min(sv9),
          tolerance = 1e-12) ## the same (typically down to 2.22e-16)
kappa(h9, exact = TRUE) / kappa(h9)  # 0.677 (i.e., rel.error = 32\%)

## Exact kappa for rectangular matrix
## panmagic.6npm1(7) :
pm7 <- rbind(c( 1, 13, 18, 23, 35, 40, 45),
             c(37, 49,  5, 10, 15, 27, 32),
             c(24, 29, 41, 46,  2, 14, 19),
             c(11, 16, 28, 33, 38, 43,  6),
             c(47,  3,  8, 20, 25, 30, 42),
             c(34, 39, 44,  7, 12, 17, 22),
             c(21, 26, 31, 36, 48,  4,  9))

kappa(pm7, exact=TRUE, norm="1") # no problem for square matrix

m76 <- pm7[,1:6]
(m79 <- cbind(pm7, 50:56, 63:57))

## Moore-Penrose inverse { ~= MASS::ginv(); differing tol (value & meaning)}:
## pinv := p(seudo) inv(erse)
pinv <- function(X, s = svd(X), tol = 64*.Machine$double.eps) {
    if (is.complex(X))
        s$u <- Conj(s$u)
    dx <- dim(X)
    ## X = U D V' ==> Result =  V {1/D} U'
    pI <- function(u,d,v) tcrossprod(v, u / rep(d, each = dx[1L]))
    pos <- (d <- s$d) > max(tol * max(dx) * d[1L], 0)
    if (all(pos))
        pI(s$u, d, s$v)
    else if (!any(pos))
        array(0, dX[2L:1L])
    else { # some pos, some not:
        i <- which(pos)
        pI(s$u[, i, drop = FALSE], d[i],
           s$v[, i, drop = FALSE])
    }
}

## rectangular
kappa(m76, norm="1")
try( kappa(m76, exact=TRUE, norm="1") )# error in  solve().. must be square

## ==> use pseudo-inverse instead of solve() for rectangular {and norm != "2"}:
iZ <- pinv(m76)
kappa(m76, exact=TRUE, norm="1", inv_z = iZ)
kappa(m76, exact=TRUE, norm="M", inv_z = iZ)
kappa(m76, exact=TRUE, norm="I", inv_z = iZ)

iX <- pinv(m79)
kappa(m79, exact=TRUE, norm="1", inv_z = iX)
kappa(m79, exact=TRUE, norm="M", inv_z = iX)
kappa(m79, exact=TRUE, norm="I", inv_z = iX)

## Using a more "accurate" than default inv_z [example by Cleve Moler]:
A <- rbind(c(4.1,   2.8),
           c(9.676, 6.608))
kappa(A) # -> Inf
kappa(A, exact=TRUE) # 8.675057e+15 ( 2-norm )

## now for the 1-norm :
try(kappa(A, exact=TRUE, norm = "1")) #-> Error: computationally singular
try(kappa(A, exact=TRUE, norm = "1",
          inv_z = solve(A, tol = 1e-19))) # 5.22057e16 on x86_64 Linux with GCC
}
\keyword{math}
