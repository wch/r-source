\name{poly}
\title{Compute Orthogonal Polynomials}
\usage{
poly(x, degree=1, oldx = NULL)
}
\alias{poly}
\description{
  Returns of evaluates orthogonal polynomials of degree 1 to
  \code{degree} over the specified set of points \code{x}. These are all
  orthogonal to the constant polynomial of degree 0.
}
\arguments{
 \item{x}{a numeric vector at which to evaluate the polynomial}
 \item{degree}{the degree of the polynomial}
 \item{oldx}{for prediction, the original data.}
}
\value{
  A matrix with rows corresponding to points in \code{x} and columns
  corresponding to the degree, with attributes \code{"degree"} specifying
  the degrees of the columns and \code{"coefs"} which contains the
  centring and normalization constants used in constructing the
  orthogonal polynomials.

  The matrix is given class \code{"poly"} as from \R 1.5.0.
}
\author{B.D. Ripley}

\note{This routine is intended for statistical purposes such as
    \code{contr.poly}: it does not attempt to orthogonalize to
    machine accuracy.
}

\seealso{\code{\link{contr.poly}}}

\examples{
poly(1:10, 3)
poly(seq(2, 4, 0.5), 3, oldx = 1:10)
}
\keyword{math}
