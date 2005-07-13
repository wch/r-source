\name{nlminb}
\alias{nlminb}
\title{Optimization using PORT routines }
\description{
  Unconstrained and constrained optimization using PORT routines.
}
\usage{
nlminb(start, objective, gradient = NULL, hessian = NULL,
       scale = 1, control = list(), lower = -Inf, upper = Inf, \dots)
}
\arguments{
  \item{start}{
    numeric vector, initial values for the parameters to be optimized
  }
  \item{objective}{
    function to be minimized.  Must return a scalar value (possibly
    NA/Inf).  The first argument to \code{objective} is the vector of
    parameters to be optimized, whose initial values are supplied
    through \code{start}.  Further arguments (fixed during the course of
    the optimization) to \code{objective} may be specified as well (see
    \code{\dots}).
  }
  \item{gradient}{
    optional function that takes the same arguments as \code{objective} and
    evaluates the gradient of \code{objective} at its first argument.  Must
    return a vector as long as \code{start}.
  }
  \item{hessian}{
    optional function that takes the same arguments as \code{objective} and
    evaluates the hessian of \code{objective} at its first argument.  Must
    return a square matrix of order \code{length(start)}.  Only the
    lower triangle is used.
  }
  \item{scale}{ See PORT documentation (or leave alone) }
  \item{control}{a list of control parameters. See below for details.}
  \item{lower, upper}{
    vector of lower and upper bounds, replicated to be as long as
    \code{start}.  If unspecified, all parameters are assumed to be
    unconstrained.
  }
  \item{\dots}{ further arguments to be supplied to \code{objective}  }
}
\value{
  A list with components:
  \item{par}{The best set of parameters found.}
  \item{objective}{The value of \code{objective} corresponding to \code{par}.}
  \item{convergence}{An integer code. \code{0} indicates successful
    convergence.
  }
  \item{message}{
    A character string giving any additional information returned by the
    optimizer, or \code{NULL}. For details, see PORT documentation.
  }
  \item{iterations}{Number of iterations performed.}
  \item{evaluations}{Number of objective function and gradient function evaluations}
}
\section{Control parameters}{
  Possible names in the \code{control} list and their default values
  are:
  \describe{
    \item{\code{eval.max}}{Maximum number of evaluations of the objective
      function allowed.  Defaults to 200.}
    \item{\code{iter.max}}{Maximum number of iterations allowed.
      Defaults to 150.}
    \item{\code{trace}}{The value of the objective function and the parameters
      is printed every trace'th iteration.  Defaults to 0 which
      indicates no trace information is to be printed.}
    \item{\code{abs.tol}}{Absolute tolerance.  Defaults to \code{1e-20}.}
    \item{\code{rel.tol}}{Relative tolerance.  Defaults to \code{1e-10}.}
    \item{\code{x.tol}}{X tolerance.  Defaults to \code{1.5e-8}.}
    \item{\code{step.min}}{Minimum step size.  Defaults to \code{2.2e-14}.}
%    \item{\code{step.max}}{Defaults to \code{1}.}
%    \item{\code{scale.init}}{Defaults to \code{-1}.}
%    \item{\code{sing.tol}}{Defaults to \code{1e-10}.}
%    \item{\code{diff.g}}{Defaults to \code{2.2e-13}.}
  }
}
\references{ \url{http://netlib.bell-labs.com/netlib/port/} }
\author{Douglas Bates, Deepayan Sarkar}
\examples{
x <- rnbinom(100, mu = 10, size = 10)
hdev <- function(par) {
    -sum(dnbinom(x, mu = par[1], size = par[2], log = TRUE))
}
nlminb(c(9, 12), hdev)
nlminb(c(20, 20), hdev, lower = 0, upper = Inf)
nlminb(c(20, 20), hdev, lower = 0.001, upper = Inf)

## slightly modified from the S-PLUS help page for nlminb
# this example minimizes a sum of squares with known solution y
sumsq <- function( x, y) {sum((x-y)^2)}
y <- rep(1,5)
x0 <- rnorm(length(y))
nlminb( start = x0, obj = sumsq, y = y)
# now use bounds with a y that has some components outside the bounds
y <- c( 0, 2, 0, -2, 0)
nlminb( start = x0, obj = sumsq, lower = -1, upper = 1, y = y)
# try using the gradient
sumsq.g <- function(x,y) 2*(x-y)
nlminb( start = x0, obj = sumsq, grad = sumsq.g, lo = -1, up = 1, y = y)
# now use the hessian, too
sumsq.h <- function(x,y) diag(2, nrow = length(x))
nlminb(st = x0, obj = sumsq, grad = sumsq.g, hes = sumsq.h, lo = -1, up = 1, y = y)

## Rest lifted from optim help page

fr <- function(x) {   ## Rosenbrock Banana function
    x1 <- x[1]
    x2 <- x[2]
    100 * (x2 - x1 * x1)^2 + (1 - x1)^2
}
grr <- function(x) { ## Gradient of 'fr'
    x1 <- x[1]
    x2 <- x[2]
    c(-400 * x1 * (x2 - x1 * x1) - 2 * (1 - x1),
       200 *      (x2 - x1 * x1))
}
nlminb(c(-1.2,1), fr)
nlminb(c(-1.2,1), fr, grr)


flb <- function(x)
    { p <- length(x); sum(c(1, rep(4, p-1)) * (x - c(1, x[-p])^2)^2) }
## 25-dimensional box constrained
## par[24] is *not* at boundary
nlminb(rep(3, 25), flb, 
          lower=rep(2, 25),
          upper=rep(4, 25)) 
}
\keyword{optimize}

