\name{optim}
\alias{optim}
\title{General-purpose Optimization}
\concept{minimization}
\concept{maximization}
\description{
  General-purpose optimization based on Nelder--Mead, quasi-Newton and
  conjugate-gradient algorithms. It includes an option for
  box-constrained optimization and simulated annealing.
}
\usage{
optim(par, fn, gr = NULL,
      method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN"),
      lower = -Inf, upper = Inf,
      control = list(), hessian = FALSE, \dots)
}
\arguments{
 \item{par}{Initial values for the parameters to be optimized over.}
 \item{fn}{A function to be minimized (or maximized), with first
   argument the vector of parameters over which minimization is to take
   place.  It should return a scalar result.}
 \item{gr}{A function to return the gradient for the \code{"BFGS"},
   \code{"CG"} and \code{"L-BFGS-B"} methods.  If it is \code{NULL}, a
   finite-difference approximation will be used.

   For the \code{"SANN"} method it specifies a function to generate a new
   candidate point.  If it is \code{NULL} a default Gaussian Markov
   kernel is used.
 }
 \item{method}{The method to be used. See \bold{Details}.}
 \item{lower, upper}{Bounds on the variables for the \code{"L-BFGS-B"} method.}
 \item{control}{A list of control parameters. See \bold{Details}.}
 \item{hessian}{Logical. Should a numerically differentiated Hessian
   matrix be returned?}
 \item{\dots}{Further arguments to be passed to \code{fn} and \code{gr}.
   Beware of partial matching to earlier arguments.}
}
\details{
  By default this function performs minimization, but it will maximize
  if \code{control$fnscale} is negative.

  The default method is an implementation of that of Nelder and Mead
  (1965), that uses only function values and is robust but relatively
  slow.  It will work reasonably well for non-differentiable functions.

  Method \code{"BFGS"} is a quasi-Newton method (also known as a variable
  metric algorithm), specifically that published simultaneously in 1970
  by Broyden, Fletcher, Goldfarb and Shanno.  This uses function values
  and gradients to build up a picture of the surface to be optimized.

  Method \code{"CG"} is a conjugate gradients method based on that by
  Fletcher and Reeves (1964) (but with the option of Polak--Ribiere or
  Beale--Sorenson updates).  Conjugate gradient methods will generally
  be more fragile than the BFGS method, but as they do not store a
  matrix they may be successful in much larger optimization problems.

  Method \code{"L-BFGS-B"} is that of Byrd \emph{et. al.} (1995) which
  allows \emph{box constraints}, that is each variable can be given a lower
  and/or upper bound. The initial value must satisfy the constraints.
  This uses a limited-memory modification of the BFGS quasi-Newton
  method. If non-trivial bounds are supplied, this method will be
  selected, with a warning.

  Nocedal and Wright (1999) is a comprehensive reference for the
  previous three methods.

  Method \code{"SANN"} is by default a variant of simulated annealing
  given in Belisle (1992). Simulated-annealing belongs to the class of
  stochastic global optimization methods. It uses only function values
  but is relatively slow. It will also work for non-differentiable
  functions. This implementation uses the Metropolis function for the
  acceptance probability. By default the next candidate point is
  generated from a Gaussian Markov kernel with scale proportional to the
  actual temperature. If a function to generate a new candidate point is
  given, method \code{"SANN"} can also be used to solve combinatorial
  optimization problems. Temperatures are decreased according to the
  logarithmic cooling schedule as given in Belisle (1992, p. 890);
  specifically, the temperature is set to
  \code{temp / log(((t-1) \%/\% tmax)*tmax + exp(1))}, where \code{t} is
  the current iteration step and \code{temp} and \code{tmax} are
  specifiable via \code{control}, see below.  Note that the
  \code{"SANN"} method depends critically on the settings of the control
  parameters. It is not a general-purpose method but can be very useful
  in getting to a good value on a very rough surface.

  Function \code{fn} can return \code{NA} or \code{Inf} if the function
  cannot be evaluated at the supplied value, but the initial value must
  have a computable finite value of \code{fn}.
  (Except for method \code{"L-BFGS-B"} where the values should always be
  finite.)

  \code{optim} can be used recursively, and for a single parameter
  as well as many.

  The \code{control} argument is a list that can supply any of the
  following components:
  \describe{
    \item{\code{trace}}{Non-negative integer. If positive,
      tracing information on the
      progress of the optimization is produced. Higher values may
      produce more tracing information: for method \code{"L-BFGS-B"}
      there are six levels of tracing.  (To understand exactly what
      these do see the source code: higher levels give more detail.)}
    \item{\code{fnscale}}{An overall scaling to be applied to the value
      of \code{fn} and \code{gr} during optimization. If negative,
      turns the problem into a maximization problem. Optimization is
      performed on \code{fn(par)/fnscale}.}
    \item{\code{parscale}}{A vector of scaling values for the parameters.
	Optimization is performed on \code{par/parscale} and these should be
	comparable in the sense that a unit change in any element produces
	about a unit change in the scaled value.}
    \item{\code{ndeps}}{A vector of step sizes for the finite-difference
      approximation to the gradient, on \code{par/parscale}
      scale. Defaults to \code{1e-3}.}
    \item{\code{maxit}}{The maximum number of iterations. Defaults to
      \code{100} for the derivative-based methods, and
      \code{500} for \code{"Nelder-Mead"}. For \code{"SANN"}
      \code{maxit} gives the total number of function evaluations. There is
      no other stopping criterion. Defaults to \code{10000}.}
    \item{\code{abstol}}{The absolute convergence tolerance. Only
      useful for non-negative functions, as a tolerance for reaching zero.}
    \item{\code{reltol}}{Relative convergence tolerance.  The algorithm
      stops if it is unable to reduce the value by a factor of
      \code{reltol * (abs(val) + reltol)} at a step.  Defaults to
      \code{sqrt(.Machine$double.eps)}, typically about \code{1e-8}.}
    \item{\code{alpha}, \code{beta}, \code{gamma}}{Scaling parameters
      for the \code{"Nelder-Mead"} method. \code{alpha} is the reflection
      factor (default 1.0), \code{beta} the contraction factor (0.5) and
      \code{gamma} the expansion factor (2.0).}
    \item{\code{REPORT}}{The frequency of reports for the \code{"BFGS"}
      and \code{"L-BFGS-B"} methods if \code{control$trace} is positive.
      Defaults to every 10 iterations.}
    \item{\code{type}}{for the conjugate-gradients method. Takes value
      \code{1} for the Fletcher--Reeves update, \code{2} for
      Polak--Ribiere and \code{3} for Beale--Sorenson.}
    \item{\code{lmm}}{is an integer giving the number of BFGS updates
      retained in the \code{"L-BFGS-B"} method, It defaults to \code{5}.}
    \item{\code{factr}}{controls the convergence of the \code{"L-BFGS-B"}
      method. Convergence occurs when the reduction in the objective is
      within this factor of the machine tolerance. Default is \code{1e7},
      that is a tolerance of about \code{1e-8}.}
    \item{\code{pgtol}}{helps control the convergence of the \code{"L-BFGS-B"}
      method. It is a tolerance on the projected gradient in the current
      search direction. This defaults to zero, when the check is
      suppressed.}
    \item{\code{temp}}{controls the \code{"SANN"} method. It is the
      starting temperature for the cooling schedule. Defaults to
      \code{10}.}
    \item{\code{tmax}}{is the number of function evaluations at each
      temperature for the \code{"SANN"} method. Defaults to \code{10}.}
  }

  Any names given to \code{par} will be copied to the vectors passed to
  \code{fn} and \code{gr}.
}
\value{
  A list with components:
  \item{par}{The best set of parameters found.}
  \item{value}{The value of \code{fn} corresponding to \code{par}.}
  \item{counts}{A two-element integer vector giving the number of calls
    to \code{fn} and \code{gr} respectively. This excludes those calls needed
    to compute the Hessian, if requested, and any calls to \code{fn} to
    compute a finite-difference approximation to the gradient.}
  \item{convergence}{An integer code. \code{0} indicates successful
    convergence. Error codes are
    \describe{
      \item{\code{1}}{indicates that the iteration limit \code{maxit}
      had been reached.}
      \item{\code{10}}{indicates degeneracy of the Nelder--Mead simplex.}
      \item{\code{51}}{indicates a warning from the \code{"L-BFGS-B"}
      method; see component \code{message} for further details.}
      \item{\code{52}}{indicates an error from the \code{"L-BFGS-B"}
      method; see component \code{message} for further details.}
    }
  }
  \item{message}{A character string giving any additional information
    returned by the optimizer, or \code{NULL}.}
  \item{hessian}{Only if argument \code{hessian} is true. A symmetric
    matrix giving an estimate of the Hessian at the solution found. Note
    that this is the Hessian of the unconstrained problem even if the
    box constraints are active.}
}
\note{
  \code{optim} will work with one-dimensional \code{par}s, but the
  default method does not work well (and will warn).  Use
  \code{\link{optimize}} instead.
}
\source{
  The code for methods \code{"Nelder-Mead"}, \code{"BFGS"} and
  \code{"CG"} was based originally on Pascal code in Nash (1990) that was
  translated by \code{p2c} and then hand-optimized.  Dr Nash has agreed
  that the code can be made freely available.

  The code for method \code{"L-BFGS-B"} is based on Fortran code by Zhu,
  Byrd, Lu-Chen and Nocedal obtained from Netlib (file
  \file{opt/lbfgs\_bcm.shar}: another version is in \file{toms/778}).

  The code for method \code{"SANN"} was contributed by A. Trapletti.
}
\references{
  Belisle, C. J. P. (1992) Convergence theorems for a class of simulated
  annealing algorithms on \eqn{R^d}{Rd}. \emph{J Applied Probability},
  \bold{29}, 885--895.

  Byrd, R. H., Lu, P., Nocedal, J. and Zhu, C.  (1995) A limited
  memory algorithm for bound constrained optimization.
  \emph{SIAM J. Scientific Computing}, \bold{16}, 1190--1208.

  Fletcher, R. and Reeves, C. M. (1964) Function minimization by
  conjugate gradients. \emph{Computer Journal} \bold{7}, 148--154.

  Nash, J. C. (1990) \emph{Compact Numerical Methods for
    Computers. Linear Algebra and Function Minimisation.} Adam Hilger.

  Nelder, J. A. and Mead, R. (1965) A simplex algorithm for function
  minimization. \emph{Computer Journal} \bold{7}, 308--313.

  Nocedal, J. and Wright, S. J. (1999) \emph{Numerical Optimization}.
  Springer.
}

\seealso{
  \code{\link{nlm}}, \code{\link{nlminb}}.

  \code{\link{optimize}} for one-dimensional minimization and
  \code{\link{constrOptim}} for constrained optimization.
}

\examples{
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
optim(c(-1.2,1), fr)
optim(c(-1.2,1), fr, grr, method = "BFGS")
optim(c(-1.2,1), fr, NULL, method = "BFGS", hessian = TRUE)
optim(c(-1.2,1), fr, grr, method = "CG")
optim(c(-1.2,1), fr, grr, method = "CG", control=list(type=2))
optim(c(-1.2,1), fr, grr, method = "L-BFGS-B")

flb <- function(x)
    { p <- length(x); sum(c(1, rep(4, p-1)) * (x - c(1, x[-p])^2)^2) }
## 25-dimensional box constrained
optim(rep(3, 25), flb, NULL, "L-BFGS-B",
      lower=rep(2, 25), upper=rep(4, 25)) # par[24] is *not* at boundary

## "wild" function , global minimum at about -15.81515
fw <- function (x)
    10*sin(0.3*x)*sin(1.3*x^2) + 0.00001*x^4 + 0.2*x+80
plot(fw, -50, 50, n=1000, main = "optim() minimising 'wild function'")

res <- optim(50, fw, method="SANN",
             control=list(maxit=20000, temp=20, parscale=20))
res
## Now improve locally
(r2 <- optim(res$par, fw, method="BFGS"))
points(r2$par, r2$val, pch = 8, col = "red", cex = 2)

## Combinatorial optimization: Traveling salesman problem
library(stats) # normally loaded

eurodistmat <- as.matrix(eurodist)

distance <- function(sq) {  # Target function
    sq2 <- embed(sq, 2)
    return(sum(eurodistmat[cbind(sq2[,2],sq2[,1])]))
}

genseq <- function(sq) {  # Generate new candidate sequence
    idx <- seq(2, NROW(eurodistmat)-1, by=1)
    changepoints <- sample(idx, size=2, replace=FALSE)
    tmp <- sq[changepoints[1]]
    sq[changepoints[1]] <- sq[changepoints[2]]
    sq[changepoints[2]] <- tmp
    return(sq)
}

sq <- c(1,2:NROW(eurodistmat),1)  # Initial sequence
distance(sq)

set.seed(2222) # chosen to get a good soln quickly
res <- optim(sq, distance, genseq, method="SANN",
             control = list(maxit=6000, temp=2000, trace=TRUE))
res  # Near optimum distance around 12842

loc <- cmdscale(eurodist)
rx <- range(x <- loc[,1])
ry <- range(y <- -loc[,2])
tspinit <- loc[sq,]
tspres <- loc[res$par,]
s <- seq(NROW(tspres)-1)

plot(x, y, type="n", asp=1, xlab="", ylab="",
     main="initial solution of traveling salesman problem")
arrows(tspinit[s,1], -tspinit[s,2], tspinit[s+1,1], -tspinit[s+1,2],
       angle=10, col="green")
text(x, y, labels(eurodist), cex=0.8)

plot(x, y, type="n", asp=1, xlab="", ylab="",
     main="optim() 'solving' traveling salesman problem")
arrows(tspres[s,1], -tspres[s,2], tspres[s+1,1], -tspres[s+1,2],
       angle=10, col="red")
text(x, y, labels(eurodist), cex=0.8)
}
\keyword{nonlinear}
\keyword{optimize}
