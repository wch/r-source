\name{uniroot}
\title{One Dimensional Root (Zero) Finding}
\usage{
uniroot(f, interval, \dots, lower = min(interval), upper = max(interval),
        tol = .Machine$double.eps^0.25, maxiter = 1000)
}
\alias{uniroot}
\arguments{
  \item{f}{the function for which the root is sought.}
  \item{interval}{a vector containing the end-points of the interval
    to be searched for the root.}
  \item{\dots}{additional named or unnamed arguments to be passed
    to \code{f}}
  \item{lower}{the lower end point of the interval to be searched.}
  \item{upper}{the upper end point of the interval to be searched.}
  \item{tol}{the desired accuracy (convergence tolerance).}
  \item{maxiter}{the maximum number of iterations.}
}
\description{
  The function \code{uniroot} searches the interval from \code{lower}
  to \code{upper} for a root (i.e., zero) of the function \code{f} with
  respect to its first argument.
}
\details{
  Note that arguments after \code{\dots} must be matched exactly.
  
  Either \code{interval} or both \code{lower} and \code{upper} must be
  specified: the upper endpoint must be strictly larger than the lower
  endpoint.  The function values at the endpoints must be of opposite
  signs (or zero).

  The function uses Fortran subroutine \file{"zeroin"} (from Netlib)
  based on algorithms given in the reference below.  They assume a
  continuous function (which then is known to have at least one root in
  the interval).

  Convergence is declared either if \code{f(x) == 0} or the change in
  \code{x} for one step of the algorithm is less than \code{tol} (plus an
  allowance for representation error in \code{x}).

  If the algorithm does not converge in \code{maxiter} steps, a warning
  is printed and the current approximation is returned.

  \code{f} will be called as \code{f(\var{x}, ...)} for a numeric value
  of \var{x}.
}
\value{
  A list with four components: \code{root} and \code{f.root} give the
  location of the root and the value of the function evaluated at that
  point. \code{iter} and \code{estim.prec} give the number of iterations
  used and an approximate estimated precision for \code{root}.  (If the
  root occurs at one of the endpoints, the estimated precision is
  \code{NA}.)
}
\source{
  Based on \file{zeroin.c} in \url{http://www.netlib.org/c/brent.shar}.
}
\references{
  Brent, R. (1973)
  \emph{Algorithms for Minimization without Derivatives.}
  Englewood Cliffs, NJ: Prentice-Hall.
}
\seealso{
  \code{\link{polyroot}} for all complex roots of a polynomial;
  \code{\link{optimize}}, \code{\link{nlm}}.
}
\examples{
require(utils) # for str

f <- function (x,a) x - a
str(xmin <- uniroot(f, c(0, 1), tol = 0.0001, a = 1/3))
str(uniroot(function(x) x*(x^2-1) + .5, lower = -2, upper = 2, tol = 0.0001),
    dig = 10)
str(uniroot(function(x) x*(x^2-1) + .5, lower = -2, upper = 2, tol = 1e-10 ),
    dig = 10)

## Find the smallest value x for which exp(x) > 0 (numerically):
r <- uniroot(function(x) 1e80*exp(x)-1e-300, c(-1000,0), tol = 1e-15)
str(r, digits= 15) ##> around -745, depending on the platform.

exp(r$root)	# = 0, but not for r$root * 0.999...
minexp <- r$root * (1 - 10*.Machine$double.eps)
exp(minexp)	# typically denormalized
}
\keyword{optimize}
