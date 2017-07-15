% File src/library/stats/man/poly.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{poly}
\title{Compute Orthogonal Polynomials}
\alias{poly}
\alias{polym}
\alias{predict.poly}
\alias{makepredictcall.poly}
\usage{
poly(x, \dots, degree = 1, coefs = NULL, raw = FALSE, simple = FALSE)
polym  (\dots, degree = 1, coefs = NULL, raw = FALSE)

\method{predict}{poly}(object, newdata, \dots)
}
\description{
  Returns or evaluates orthogonal polynomials of degree 1 to
  \code{degree} over the specified set of points \code{x}: these are all
  orthogonal to the constant polynomial of degree 0.  Alternatively,
  evaluate raw polynomials.
}
\arguments{
 \item{x, newdata}{a numeric vector at which to evaluate the
   polynomial. \code{x} can also be a matrix.  Missing values are not
   allowed in \code{x}.}
 \item{degree}{the degree of the polynomial.  Must be less than the
   number of unique points when \code{raw} is false, as by default.}
 \item{coefs}{for prediction, coefficients from a previous fit.}
 \item{raw}{if true, use raw and not orthogonal polynomials.}
 \item{simple}{logical indicating if a simple matrix (with no further
   \code{\link{attributes}} but \code{\link{dimnames}}) should be
   returned.  For speedup only.}
 \item{object}{an object inheriting from class \code{"poly"}, normally
   the result of a call to \code{poly} with a single vector argument.}
 \item{\dots}{\code{poly}, \code{polym}: further vectors.\cr
   \code{predict.poly}: arguments to be passed to or from other methods.
 }
}
\value{
  For \code{poly} and \code{polym()} (when \code{simple=FALSE} and
  \code{coefs=NULL} as per default):\cr
  A matrix with rows corresponding to points in \code{x} and columns
  corresponding to the degree, with attributes \code{"degree"} specifying
  the degrees of the columns and (unless \code{raw = TRUE})
  \code{"coefs"} which contains the centering and normalization
  constants used in constructing the orthogonal polynomials and
  class \code{c("poly", "matrix")}.

  For \code{poly(*, simple=TRUE)}, \code{polym(*, coefs=<non-NULL>)},
  and \code{predict.poly()}: a matrix.
}
\details{
  Although formally \code{degree} should be named (as it follows
  \code{\dots}), an unnamed second argument of length 1 will be
  interpreted as the degree, such that \code{poly(x, 3)} can be used in
  formulas.

  The orthogonal polynomial is summarized by the coefficients, which can
  be used to evaluate it via the three-term recursion given in Kennedy
  & Gentle (1980, pp.\sspace{}343--4), and used in the \code{predict} part of
  the code.

  \code{poly} using \code{\dots} is just a convenience wrapper for
  \code{polym}: \code{coef} is ignored.  Conversely, if \code{polym} is
  called with a single argument in \code{\dots} it is a wrapper for
  \code{poly}.
}
\note{
  This routine is intended for statistical purposes such as
  \code{contr.poly}: it does not attempt to orthogonalize to
  machine accuracy.
}
\author{
  R Core Team.  Keith Jewell (Campden BRI Group, UK) contributed
  improvements for correct prediction on subsets.
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992)
  \emph{Statistical Models in S}.
  Wadsworth & Brooks/Cole.

  Kennedy, W. J. Jr and Gentle, J. E. (1980)
  \emph{Statistical Computing} Marcel Dekker.
}

\seealso{
  \code{\link{contr.poly}}.

  \code{\link{cars}} for an example of polynomial regression.
}

\examples{
od <- options(digits = 3) # avoid too much visual clutter
(z <- poly(1:10, 3))
predict(z, seq(2, 4, 0.5))
zapsmall(poly(seq(4, 6, 0.5), 3, coefs = attr(z, "coefs")))

 zm <- zapsmall(polym (    1:4, c(1, 4:6),  degree = 3)) # or just poly():
(z1 <- zapsmall(poly(cbind(1:4, c(1, 4:6)), degree = 3)))
## they are the same :
stopifnot(all.equal(zm, z1, tol = 1e-15))

## poly(<matrix>, df) --- used to fail till July 14 (vive la France!), 2017:
m2 <- cbind(1:4, c(1, 4:6))
pm2 <- zapsmall(poly(m2, 3)) # "unnamed degree = 3"
stopifnot(all.equal(pm2, zm, tol = 1e-15))

options(od)
}
\keyword{math}
