\name{uniroot}
\title{One Dimensional Root Finding}
\usage{
uniroot(f, interval, lower = min(interval), upper = max(interval),
        tol = .Machine$double.eps^0.25, ...)
}
\alias{uniroot}
\arguments{
  \item{f}{the function for which the root is sought.}
  \item{interval}{a vector containing the end-points of the interval
    to be searched for the root.}
  \item{lower}{the lower end point of the interval to be searched.}
  \item{upper}{the upper end point of the interval to be searched.}
  \item{tol}{the desired accuracy.}
  \item{...}{additional arguments to \code{f}.}
}
\description{
  The function \code{optimize} searches the interval from \code{lower}
  to \code{upper} for a zero of the function \code{f} with respect to
  its first argument.

  The function uses Fortran code (from Netlib) based on algorithms given
  in the reference below.
}
\value{
  A list with components \code{root} and \code{f.root} which give the
  location of the root and the value of the function evaluated at that
  point.
}
\references{
  Brent, R. (1973).
  \emph{Algorithms for Minimization without Derivatives}.
  Englewood Cliffs, NJ: Prentice-Hall.
}
\seealso{
  \code{\link{polyroot}} for all complex roots of a polynomial;
  \code{\link{optimize}}, \code{\link{nlm}}.
}
\examples{
f <- function (x,a) x - a
xmin <- uniroot(f, c(0, 1), tol = 0.0001, a = 1/3)
xmin
}
\keyword{optimize}
