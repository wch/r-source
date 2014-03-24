% File src/library/stats/man/poly.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\newcommand{\sspace}{\ifelse{latex}{\out{~}}{ }}

\name{poly}
\alias{poly}
\alias{polym}
\alias{predict.poly}
\alias{makepredictcall.poly}
\title{Compute Orthogonal Polynomials}
\usage{
poly(x, \dots, degree = 1, coefs = NULL, raw = FALSE)
polym(\dots, degree = 1, raw = FALSE)

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
   number of unique points if \code{raw = TRUE}.}
 \item{coefs}{for prediction, coefficients from a previous fit.}
 \item{raw}{if true, use raw and not orthogonal polynomials.}
 \item{object}{an object inheriting from class \code{"poly"}, normally
   the result of a call to \code{poly} with a single vector argument.}
 \item{\dots}{\code{poly}, \code{polym}: further vectors.\cr
   \code{predict.poly}: arguments to be passed to or from other methods.
 }
}
\value{
  For \code{poly} with a single vector argument:\cr
  A matrix with rows corresponding to points in \code{x} and columns
  corresponding to the degree, with attributes \code{"degree"} specifying
  the degrees of the columns and (unless \code{raw = TRUE})
  \code{"coefs"} which contains the centering and normalization
  constants used in constructing the orthogonal polynomials.  The matrix
  has given class \code{c("poly", "matrix")}.

  For \code{poly} and \code{polym} with more than one input, and
  \code{predict.poly}: a matrix.
}
\details{
  Although formally \code{degree} should be named (as it follows
  \code{\dots}), an unnamed second argument of length 1 will be
  interpreted as the degree.

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

zapsmall(polym(1:4, c(1, 4:6), degree = 3)) # or just poly()
zapsmall(poly(cbind(1:4, c(1, 4:6)), degree = 3))
options(od)
}
\keyword{math}
