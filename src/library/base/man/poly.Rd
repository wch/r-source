\name{poly}
\alias{poly}
\alias{predict.poly}
\alias{makepredictcall.poly}
\title{Compute Orthogonal Polynomials}
\usage{
poly(x, degree = 1, coefs = NULL)
\method{predict}{poly}(object, newdata, \dots)
}
\description{
  Returns of evaluates orthogonal polynomials of degree 1 to
  \code{degree} over the specified set of points \code{x}. These are all
  orthogonal to the constant polynomial of degree 0.
}
\arguments{
 \item{x, newdata}{a numeric vector at which to evaluate the polynomial}
 \item{degree}{the degree of the polynomial}
 \item{coefs}{for prediction, coefficients from a previous fit.}
 \item{object}{an object inheriting from class \code{"poly"}.}
 \item{\dots}{arguments to be passed to or from other methods.}
}
\value{
  A matrix with rows corresponding to points in \code{x} and columns
  corresponding to the degree, with attributes \code{"degree"} specifying
  the degrees of the columns and \code{"coefs"} which contains the
  centring and normalization constants used in constructing the
  orthogonal polynomials.

  The matrix is given class \code{c("poly", "matrix")} as from \R 1.5.0.
}
\details{
  The orthogonal polynomial is summarized by the coefficients, which can
  be used to evaluate it via the three-term recursion given in Kennedy
  \& Gentle (1980, pp. 343-4), and use in the ``predict'' part of the code.
}
\author{B. D. Ripley}

\note{
  This routine is intended for statistical purposes such as
  \code{contr.poly}: it does not attempt to orthogonalize to
  machine accuracy.
}
\references{
  Kennedy, W. J. Jr and Gentle, J. E. (1980)
  \emph{Statistical Computing} Marcel Dekker.
}

\seealso{\code{\link{contr.poly}}}

\examples{
(z <- poly(1:10, 3))
predict(z, seq(2, 4, 0.5))
poly(seq(4, 6, 0.5), 3, coefs = attr(z, "coefs"))
}
\keyword{math}
