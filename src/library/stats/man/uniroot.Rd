% File src/library/stats/man/uniroot.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{uniroot}
\title{One Dimensional Root (Zero) Finding}
\alias{uniroot}
\usage{
uniroot(f, interval, \dots,
        lower = min(interval), upper = max(interval),
        f.lower = f(lower, \dots), f.upper = f(upper, \dots),
        extendInt = c("no", "yes", "downX", "upX"), check.conv = FALSE,
        tol = .Machine$double.eps^0.25, maxiter = 1000, trace = 0)
}
\arguments{
  \item{f}{the function for which the root is sought.}
  \item{interval}{a vector containing the end-points of the interval
    to be searched for the root.}
  \item{\dots}{additional named or unnamed arguments to be passed
    to \code{f}}
  \item{lower, upper}{the lower and upper end points of the interval to
    be searched.}
  \item{f.lower, f.upper}{the same as \code{f(upper)} and
    \code{f(lower)}, respectively.  Passing these values from the caller
    where they are often known is more economical as soon as \code{f()}
    contains non-trivial computations.}
  \item{extendInt}{character string specifying if the interval
    \code{c(lower,upper)} should be extended or directly produce an error
    when \code{f()} does not have differing signs at the endpoints.  The
    default, \code{"no"}, keeps the search interval and hence produces
    an error.  Can be abbreviated.}
  \item{check.conv}{logical indicating whether a convergence warning of the
    underlying \code{\link{uniroot}} should be caught as an error and if
    non-convergence in \code{maxiter} iterations should be an error
    instead of a warning.}
  \item{tol}{the desired accuracy (convergence tolerance).}
  \item{maxiter}{the maximum number of iterations.}
  \item{trace}{integer number; if positive, tracing information is
    produced.  Higher values giving more details.}
}
\description{
  The function \code{uniroot} searches the interval from \code{lower}
  to \code{upper} for a root (i.e., zero) of the function \code{f} with
  respect to its first argument.

  Setting \code{extendInt} to a non-\code{"no"} string, means searching
  for the correct \code{interval = c(lower,upper)} if \code{sign(f(x))}
  does not satisfy the requirements at the interval end points; see the
  \sQuote{Details} section.
}
\details{
  Note that arguments after \code{\dots} must be matched exactly.

  Either \code{interval} or both \code{lower} and \code{upper} must be
  specified: the upper endpoint must be strictly larger than the lower
  endpoint.
  The function values at the endpoints must be of opposite signs (or
  zero), for \code{extendInt="no"}, the default.  Otherwise, if
  \code{extendInt="yes"}, the interval is extended on both sides, in
  search of a sign change, i.e., until the search interval \eqn{[l,u]}
  satisfies \eqn{f(l) \cdot f(u) \le 0}{f(l) * f(u) <= 0}.

  If it is \emph{known how} \eqn{f} changes sign at the root
  \eqn{x_0}{x0}, that is, if the function is increasing or decreasing there,
  \code{extendInt} can (and typically should) be specified as
  \code{"upX"} (for \dQuote{upward crossing}) or \code{"downX"},
  respectively.  Equivalently, define \eqn{S := \pm 1}{S:= +/- 1}, to
  require \eqn{S = \mathrm{sign}(f(x_0 + \epsilon))}{S = sign(f(x0 +
    eps))} at the solution.  In that case, the search interval \eqn{[l,u]}
  possibly is extended to be such that \eqn{S\cdot f(l)\le 0}{%
    S * f(l) <= 0} and \eqn{S \cdot f(u) \ge 0}{S * f(u) >= 0}.

  \code{uniroot()} uses Fortran subroutine \file{"zeroin"} (from Netlib)
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

  The argument passed to \code{f} has special semantics and used to be
  shared between calls.  The function should not copy it.
}

\value{
  A list with at least four components: \code{root} and \code{f.root}
  give the location of the root and the value of the function evaluated
  at that point. \code{iter} and \code{estim.prec} give the number of
  iterations used and an approximate estimated precision for
  \code{root}.  (If the root occurs at one of the endpoints, the
  estimated precision is \code{NA}.)

  Further components may be added in future: component \code{init.it}
  was added in \R 3.1.0.  
}
%\author{of \code{extendInt} and \code{check.conv}: Martin Maechler.}% originally as
% safeUroot() in CRAN packages nor1mix and copula
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
\examples{\donttest{
require(utils) # for str

## some platforms hit zero exactly on the first step:
## if so the estimated precision is 2/3.
f <- function (x, a) x - a
str(xmin <- uniroot(f, c(0, 1), tol = 0.0001, a = 1/3))

## handheld calculator example: fixed point of cos(.):
uniroot(function(x) cos(x) - x, lower = -pi, upper = pi, tol = 1e-9)$root

str(uniroot(function(x) x*(x^2-1) + .5, lower = -2, upper = 2,
            tol = 0.0001))
str(uniroot(function(x) x*(x^2-1) + .5, lower = -2, upper = 2,
            tol = 1e-10))

## Find the smallest value x for which exp(x) > 0 (numerically):
r <- uniroot(function(x) 1e80*exp(x) - 1e-300, c(-1000, 0), tol = 1e-15)
str(r, digits.d = 15) # around -745, depending on the platform.

exp(r$root)     # = 0, but not for r$root * 0.999...
minexp <- r$root * (1 - 10*.Machine$double.eps)
exp(minexp)     # typically denormalized
}% donttest because printed output is so much platform dependent

##--- uniroot() with new interval extension + checking features: --------------

f1 <- function(x) (121 - x^2)/(x^2+1)
f2 <- function(x) exp(-x)*(x - 12)

try(uniroot(f1, c(0,10)))
try(uniroot(f2, c(0, 2)))
##--> error: f() .. end points not of opposite sign

## where as  'extendInt="yes"'  simply first enlarges the search interval:
u1 <- uniroot(f1, c(0,10),extendInt="yes", trace=1)
u2 <- uniroot(f2, c(0,2), extendInt="yes", trace=2)
stopifnot(all.equal(u1$root, 11, tolerance = 1e-5),
          all.equal(u2$root, 12, tolerance = 6e-6))

## The *danger* of interval extension:
## No way to find a zero of a positive function, but
## numerically, f(-|M|) becomes zero :
u3 <- uniroot(exp, c(0,2), extendInt="yes", trace=TRUE)

## Nonsense example (must give an error):
tools::assertCondition( uniroot(function(x) 1, 0:1, extendInt="yes"),
                       "error", verbose=TRUE)

## Convergence checking :
sinc <- function(x) ifelse(x == 0, 1, sin(x)/x)
curve(sinc, -6,18); abline(h=0,v=0, lty=3, col=adjustcolor("gray", 0.8))
\dontshow{tools::assertWarning(}
uniroot(sinc, c(0,5), extendInt="yes", maxiter=4) #-> "just" a warning
\dontshow{ , verbose=TRUE)}

## now with  check.conv=TRUE, must signal a convergence error :
\dontshow{tools::assertError(}
uniroot(sinc, c(0,5), extendInt="yes", maxiter=4, check.conv=TRUE)
\dontshow{ , verbose=TRUE)}

### Weibull cumulative hazard (example origin, Ravi Varadhan):
cumhaz <- function(t, a, b) b * (t/b)^a
froot <- function(x, u, a, b) cumhaz(x, a, b) - u

n <- 1000
u <- -log(runif(n))
a <- 1/2
b <- 1
## Find failure times
ru <- sapply(u, function(x)
   uniroot(froot, u=x, a=a, b=b, interval= c(1.e-14, 1e04),
           extendInt="yes")$root)
ru2 <- sapply(u, function(x)
   uniroot(froot, u=x, a=a, b=b, interval= c(0.01,  10),
           extendInt="yes")$root)
stopifnot(all.equal(ru, ru2, tolerance = 6e-6))

r1 <- uniroot(froot, u= 0.99, a=a, b=b, interval= c(0.01, 10),
             extendInt="up")
stopifnot(all.equal(0.99, cumhaz(r1$root, a=a, b=b)))

## An error if 'extendInt' assumes "wrong zero-crossing direction":
\dontshow{tools::assertError(}
uniroot(froot, u= 0.99, a=a, b=b, interval= c(0.1, 10), extendInt="down")
\dontshow{ , verbose=TRUE)}
}
\keyword{optimize}
