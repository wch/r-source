\name{constrOptim}
\alias{constrOptim}
\title{Linearly constrained optimisation}
\description{
Minimise a function subject to linear inequality constraints using an
adaptive barrier algorithm.  
}
\usage{
constrOptim(theta, f, grad, ui, ci, mu = 1e-04, control = list(),
            method = if(is.null(grad)) "Nelder-Mead" else "BFGS",
            outer.iterations = 100, outer.eps = 1e-05, \dots)
}
\arguments{
  \item{theta}{Starting value: must be in the feasible region.}
  \item{f}{Function to minimise.}
  \item{grad}{Gradient of \code{f}.}
  \item{ui}{Constraints (see below).}
  \item{ci}{Constraints (see below).}
  \item{mu}{(Small) tuning parameter.}
  \item{control}{Passed to \code{\link{optim}}.}
  \item{method}{Passed to \code{\link{optim}}.}
  \item{outer.iterations}{Iterations of the barrier algorithm.}
  \item{outer.eps}{Criterion for relative convergence of the barrier
    algorithm.}
  \item{\dots}{ Other arguments passed to \code{\link{optim}}, which
    will pass them to \code{f} and \code{grad} if it does not used them.}
}
\details{
  The feasible region is defined by \code{ui \%*\% theta - ci >= 0}. The
  starting value must be in the interior of the feasible region, but the
  minimum may be on the boundary. 
  
  A logarithmic barrier is added to enforce the constraints and then
  \code{\link{optim}} is called. The barrier function is chosen so that
  the objective function should decrease at each outer iteration. Minima
  in the interior of the feasible region are typically found quite
  quickly, but a substantial number of outer iterations may be needed
  for a minimum on the boundary.

 The tuning parameter \code{mu} multiplies the barrier term. Its precise
  value is often relatively unimportant. As \code{mu} increases the
  augmented objective function becomes closer to the original objective
  function but also less smooth near the boundary of the feasible
  region.

  Any \code{optim} method that permits infinite values for the objective
  function may be used (currently all but "L-BFGS-B").  The gradient
  function must be supplied except with \code{method="Nelder-Mead"}.
  
  As with \code{optim}, the default is to minimise and maximisation can
  be performed by setting \code{control$fnscale} to a negative value.
  
}
\value{
  As for \code{\link{optim}}, but with two extra components:
  \code{barrier.value} giving the value of the barrier function at the
  optimum and \code{outer.iterations} gives the
  number of outer iterations (calls to \code{optim})
}

\references{
  K. Lange \emph{Numerical Analysis for Statisticians.} Springer
  2001, p185ff
}

\seealso{
  \code{\link{optim}}, especially \code{method="L-BFGS-B"} which
  does box-constrained optimisation.
}

\examples{
## from optim
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

optim(c(-1.2,1), fr, grr)
#Box-constraint, optimum on the boundary
constrOptim(c(-1.2,0.9), fr, grr, ui=rbind(c(-1,0),c(0,-1)), ci=c(-1,-1))
#  x<=0.9,  y-x>0.1
constrOptim(c(.5,0), fr, grr, ui=rbind(c(-1,0),c(1,-1)), ci=c(-0.9,0.1))


## Solves linear and quadratic programming problems
## but needs a feasible starting value
#
# from example(solve.QP) in 'quadprog'
# no derivative
fQP <- function(b) {-sum(c(0,5,0)*b)+0.5*sum(b*b)}
Amat       <- matrix(c(-4,-3,0,2,1,0,0,-2,1),3,3)
bvec       <- c(-8,2,0)
constrOptim(c(2,-1,-1), fQP, NULL, ui=t(Amat),ci=bvec)
# derivative
gQP <- function(b) {-c(0,5,0)+b}
constrOptim(c(2,-1,-1), fQP, gQP, ui=t(Amat), ci=bvec)

## Now with maximisation instead of minimisation
hQP <- function(b) {sum(c(0,5,0)*b)-0.5*sum(b*b)}
constrOptim(c(2,-1,-1), hQP, NULL, ui=t(Amat), ci=bvec,
            control=list(fnscale=-1))
}
\keyword{optimize}

