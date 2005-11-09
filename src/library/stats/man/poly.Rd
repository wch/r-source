\name{poly}
\alias{poly}
\alias{polym}
\alias{predict.poly}
\alias{makepredictcall.poly}
\title{Compute Orthogonal Polynomials}
\usage{
poly(x, \dots, degree = 1, coefs = NULL, raw = FALSE)
polym(\dots, degree = 1), raw = FALSE

\method{predict}{poly}(object, newdata, \dots)
}
\description{
  Returns or evaluates orthogonal polynomials of degree 1 to
  \code{degree} over the specified set of points \code{x}. These are all
  orthogonal to the constant polynomial of degree 0.  Alternatively,
  evaluate raw polynomials.
}
\arguments{
 \item{x, newdata}{a numeric vector at which to evaluate the
   polynomial. \code{x} can also be a matrix.  Missing values are not
   allowed in \code{x}.}
 \item{degree}{the degree of the polynomial}
 \item{coefs}{for prediction, coefficients from a previous fit.}
 \item{raw}{if true, use raw and not orthogonal polynomials.}
 \item{object}{an object inheriting from class \code{"poly"}, normally
   the result of a call to \code{poly} with a single vector argument.}
 \item{\dots}{\code{poly, polym}: further vectors.\cr
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

  Other cases of \code{poly} and \code{polym}, and \code{predict.poly}:
  a matrix.
}
\details{
  Although formally \code{degree} should be named (as it follows
  \code{\dots}), an unnamed second argument of length 1 will be
  interpreted as the degree.
  
  The orthogonal polynomial is summarized by the coefficients, which can
  be used to evaluate it via the three-term recursion given in Kennedy
  \& Gentle (1980, pp. 343--4), and used in the \dQuote{predict} part of
  the code.
}
\note{
  This routine is intended for statistical purposes such as
  \code{contr.poly}: it does not attempt to orthogonalize to
  machine accuracy.
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992)
  \emph{Statistical Models in S}.
  Wadsworth \& Brooks/Cole.

  Kennedy, W. J. Jr and Gentle, J. E. (1980)
  \emph{Statistical Computing} Marcel Dekker.
}

\seealso{
  \code{\link{contr.poly}}.

  \code{\link{cars}} for an example of polynomial regression.
}

\examples{
(z <- poly(1:10, 3))
predict(z, seq(2, 4, 0.5))
poly(seq(4, 6, 0.5), 3, coefs = attr(z, "coefs"))

polym(1:4, c(1, 4:6), degree=3) # or just poly()
poly(cbind(1:4, c(1, 4:6)), degree=3)
}
\keyword{math}
