% File src/library/base/man/kappa.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
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
      norm = NULL, method = c("qr", "direct"), \dots)
\method{kappa}{lm}(z, \dots)
\method{kappa}{qr}(z, \dots)

.kappa_tri(z, exact = FALSE, LINPACK = TRUE, norm = NULL, \dots)

rcond(x, norm = c("O","I","1"), triangular = FALSE, \dots)
}
\arguments{
  \item{z, x}{A matrix or a the result of \code{\link{qr}} or a fit from
    a class inheriting from \code{"lm"}.}
  \item{exact}{logical.  Should the result be exact?}
  \item{norm}{character string, specifying the matrix norm with respect
    to which the condition number is to be computed, see also
    \code{\link{norm}}.  For \code{rcond}, the default is \code{"O"},
    meaning the \bold{O}ne- or 1-norm.  The (currently only) other
    possible value is \code{"I"} for the infinity norm.}
  \item{method}{character string, specifying the method to be used;
    \code{"qr"} is default for back-compatibility, mainly.}
  \item{triangular}{logical.  If true, the matrix used is just the lower
    triangular part of \code{z}.}
  \item{LINPACK}{logical.  If true and \code{z} is not complex, the
    Linpack routine \code{dtrco()} is called; otherwise the relevant
    Lapack routine is.}
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
  For \code{kappa()}, if \code{exact = FALSE} (the default) the 2-norm
  condition number is estimated by a cheap approximation.  However, the
  exact calculation (via \code{\link{svd}}) is also likely to be quick
  enough.

  Note that the 1- and Inf-norm condition numbers are much faster to
  calculate, and \code{rcond()} computes these \emph{\bold{r}eciprocal}
  condition numbers, also for complex matrices, using standard Lapack
  routines.

  \code{kappa} and \code{rcond} are different interfaces to
  \emph{partly} identical functionality.

  \code{.kappa_tri} is an internal function called by \code{kappa.qr} and
  \code{kappa.default}.
}
\value{
  The condition number, \eqn{kappa}, or an approximation if
  \code{exact = FALSE}.
}
\source{
  The LAPACK routines \code{DTRCON} and \code{ZTRCON} and the LINPACK
  routine \code{DTRCO}.

  LAPACK and LINPACK are from \url{http://www.netlib.org/lapack} and
  \url{http://www.netlib.org/linpack} and their guides are listed
  in the references.
}
\references{
  Anderson. E. and ten others (1999)
  \emph{LAPACK Users' Guide}. Third Edition. SIAM.\cr
  Available on-line at
  \url{http://www.netlib.org/lapack/lug/lapack_lug.html}.

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
kappa(x1, exact = TRUE)        # 13.68
kappa(x2 <- cbind(x1, 2:11)) # high! [x2 is singular!]

hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
sv9 <- svd(h9 <- hilbert(9))$ d
kappa(h9)  # pretty high!
kappa(h9, exact = TRUE) == max(sv9) / min(sv9)
kappa(h9, exact = TRUE) / kappa(h9)  # 0.677 (i.e., rel.error = 32\%)
}
\keyword{math}
