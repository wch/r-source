\name{optimize}
\title{One Dimensional Optimization}
\usage{
optimize(f = , interval = , lower = min(interval),
        upper = max(interval), maximum = FALSE,
        tol = .Machine$double.eps^0.25, \dots)
optimise(f = , interval = , lower = min(interval),
        upper = max(interval), maximum = FALSE,
        tol = .Machine$double.eps^0.25, \dots)
}
\alias{optimize}
\alias{optimise}
\arguments{
  \item{f}{the function to be optimized. The function is
    either minimized or maximized over its first argument
    depending on the value of \code{maximum}.}
  \item{interval}{a vector containing the end-points of the interval
    to be searched for the minimum.}
  \item{lower}{the lower end point of the interval
    to be searched.}
  \item{upper}{the upper end point of the interval
    to be searched.}
  \item{maximum}{logical. Should we maximize or minimize (the default)?}
  \item{tol}{the desired accuracy.}
  \item{\dots}{additional arguments to \code{f}.}
}
\description{
  The function \code{optimize} searches the interval from
  \code{lower} to \code{upper} for a minimum or maximum of
  the function \code{f} with respect to its first argument.

  It uses Fortran code (from Netlib) based on algorithms given in the
  reference.

  \code{optimise} is an alias for \code{optimize}.
}
\value{
  A list with components \code{minimum} (or \code{maximum})
  and \code{objective} which give the location of the minimum (or maximum)
  and the value of the function at that point.
}
\references{
  Brent, R. (1973)
  \emph{Algorithms for Minimization without Derivatives.}
  Englewood Cliffs N.J.: Prentice-Hall.
}
\seealso{
\code{\link{nlm}}, \code{\link{uniroot}}.
}
\examples{
f <- function (x,a) (x-a)^2
xmin <- optimize(f, c(0, 1), tol = 0.0001, a = 1/3)
xmin
}
\keyword{optimize}
