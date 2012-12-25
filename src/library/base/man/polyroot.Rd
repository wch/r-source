% File src/library/base/man/polyroot.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{polyroot}
\title{Find Zeros of a Real or Complex Polynomial}
\usage{
polyroot(z)
}
\alias{polyroot}
\arguments{
  \item{z}{the vector of polynomial coefficients in increasing order.}
}
\description{
  Find zeros of a real or complex polynomial.
}
\details{
  A polynomial of degree \eqn{n - 1},
  \deqn{
    p(x) = z_1 + z_2 x + \cdots + z_n x^{n-1}}{
    p(x) = z1 + z2 * x + \ldots + z[n] * x^(n-1)}
  is given by its coefficient vector \code{z[1:n]}.
  \code{polyroot} returns the \eqn{n-1} complex zeros of \eqn{p(x)}
  using the Jenkins-Traub algorithm.

  If the coefficient vector \code{z} has zeroes for the highest powers,
  these are discarded.

  There is no maximum degree, but numerical stability
  may be an issue for all but low-degree polynomials.
}
\value{
  A complex vector of length \eqn{n - 1}, where \eqn{n} is the position
  of the largest non-zero element of \code{z}.
}
\source{
  C translation by Ross Ihaka of Fortran code in the reference, with
  modifications by the R Core Team.
}
\references{
  Jenkins and Traub (1972)
  TOMS Algorithm 419.
  \emph{Comm. ACM}, \bold{15}, 97--99.
}
\seealso{
  \code{\link{uniroot}} for numerical root finding of arbitrary
  functions;
  \code{\link{complex}} and the \code{zero} example in the demos
  directory.
}
\examples{
polyroot(c(1, 2, 1))
round(polyroot(choose(8, 0:8)), 11) # guess what!
for (n1 in 1:4) print(polyroot(1:n1), digits = 4)
polyroot(c(1, 2, 1, 0, 0)) # same as the first
}
\keyword{math}
