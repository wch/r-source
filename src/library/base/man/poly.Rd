\name{poly}
\title{Compute Orthogonal Polynomials}
\usage{
poly(x, degree=1)
}
\alias{poly}
\arguments{
 \item{x}{a numeric vector at which to evaluate the polynomial}
 \item{degree}{the degree of the polynomial}
}
\description{
Returns orthogonal polynomials of degree 1 to \code{degree} over the
specified set of points \code{x}. These are all orthogonal to the
constant polynomial of degree 0.
}
\value{
  A matrix with rows corresponding to points in \code{x} and columns
  corresponding to the degree, with attribute \code{"degree"} specifying
  the degrees of the columns.}
\author{B.D. Ripley}

\seealso{\code{\link{contr.poly}}}

\examples{
poly(1:10, 3)
}
\keyword{math}
