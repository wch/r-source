\name{nls}
\alias{nls}
%\alias{anova.nls}
%\alias{anovalist.nls}
%\alias{coef.nls}
%\alias{deviance.nls}
%\alias{df.residual.nls}
%\alias{fitted.nls}
%\alias{logLik.nls}
%\alias{print.nls}
%\alias{residuals.nls}
%\alias{summary.nls}
%\alias{print.summary.nls}
%\alias{weights.nls}
%\alias{vcov.nls}
\title{Nonlinear Least Squares}
\description{
  Determine the nonlinear least-squares estimates of the nonlinear model
  parameters and return a class \code{nls} object.
}
\usage{
nls(formula, data, start, control, algorithm,
    trace, subset, weights, na.action, model, upper, lower)
}
\arguments{
 \item{formula}{a nonlinear model formula including variables and parameters}
 \item{data}{an optional data frame in which to evaluate the variables in
     \code{formula}}
 \item{start}{a named list or named numeric vector of starting estimates}
 \item{control}{an optional list of control settings.  See
     \code{nls.control} for the names of the settable control values and
     their effect.}
 \item{algorithm}{character string specifying the algorithm to use.
     The default algorithm is a Gauss-Newton algorithm. Other possible
     values are \code{"plinear"} for the Golub-Pereyra algorithm for
     partially linear least-squares models and \code{"port"} for the
     "nl2sol" algorithm from the Port package.}
 \item{trace}{logical value indicating if a trace of the iteration
     progress should be printed.  Default is \code{FALSE}.  If
     \code{TRUE} the residual sum-of-squares and the parameter values
     are printed at the conclusion of each iteration.  When the
     \code{"plinear"} algorithm is used, the conditional estimates of
     the linear parameters are printed after the nonlinear
     parameters.  When the \code{"port"} algorithms are used the
     objective function value printed is half the residual sum-of-squares.}
 \item{subset}{an optional vector specifying a subset of observations
   to be used in the fitting process.}
 \item{weights}{an optional numeric vector of (fixed) weights.  When
   present, the objective function is weighted least squares. \emph{not
     yet implemented}}
\item{na.action}{a function which indicates what should happen
    when the data contain \code{NA}s.}
\item{model}{logical.  If true, the model frame is returned as part of
  the object. Default is \code{FALSE}}
\item{lower, upper}{
    vector of lower and upper bounds, replicated to be as long as
    \code{start}.  If unspecified, all parameters are assumed to be
    unconstrained.  Bounds can only be used with the \code{"port"}
    algorithm.  They are ignored, with a warning, if given for other
    algorithms.}
}
\details{
  \bold{Do not use \code{nls} on artificial "zero-residual" data.}

  The \code{nls} function uses a relative-offset convergence criterion
  that compares the numerical imprecision at the current parameter
  estimates to the residual sum-of-squares.  This performs well on data of
  the form \deqn{y=f(x,\theta)+\epsilon}{y = f(x, theta) + eps} (with
  \code{var(eps) > 0}).  It
  fails to indicate convergence on data of the form
  \deqn{y=f(x,\theta)}{y = f(x, theta)} because the criterion amounts to
  comparing two components of the round-off error.  If you wish to test
  \code{nls} on artificial data please add a noise component, as shown
  in the example below.
  
  An \code{nls} object is a type of fitted model object.  It has methods
  for the generic functions \code{\link{coef}}, \code{\link{formula}},
  \code{\link{resid}}, \code{\link{print}}, \code{\link{summary}},
  \code{\link{AIC}}, \code{\link{fitted}} and \code{\link{vcov}}.

  Variables in \code{formula} are looked for first in \code{data}, then
  the environment of \code{formula} (added in 1.9.1) and finally along
  the search path.  Functions in \code{formula} are searched for first
  in the environment of \code{formula} (added in 1.9.1) and then along
  the search path.
}
\value{
  A list of
  \item{m}{an \code{nlsModel} object incorporating the model}
  \item{data}{the expression that was passed to \code{nls} as the data
    argument.  The actual data values are present in the environment of
    the \code{m} component.}
}
\references{
  Bates, D.M. and Watts, D.G. (1988)
  \emph{Nonlinear Regression Analysis and Its Applications},
  Wiley

  Bates, D. M. and Chambers, J. M. (1992)
  \emph{Nonlinear models.}
  Chapter 10 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth \& Brooks/Cole.

  \url{http://netlib.bell-labs.com/netlib/port/} for the Port library
}
\author{Douglas M. Bates and Saikat DebRoy}
\seealso{\code{\link{nlsModel}}}
\examples{
DNase1 <- subset(DNase, Run == 1)
## using a selfStart model
fm1DNase1 <- nls( density ~ SSlogis(log(conc), Asym, xmid, scal), DNase1)
summary( fm1DNase1 )
## using conditional linearity
fm2DNase1 <- nls( density ~ 1/(1 + exp((xmid - log(conc))/scal)),
                  data = DNase1,
                  start = list(xmid = 0, scal = 1),
                  alg = "plinear", trace = TRUE)
summary( fm2DNase1 )
## without conditional linearity
fm3DNase1 <- nls( density ~ Asym/(1 + exp((xmid - log(conc))/scal)),
                  data = DNase1,
                  start = list(Asym = 3, xmid = 0, scal = 1),
                  trace = TRUE)
summary( fm3DNase1 )
## using the nl2sol algorithm
fm4DNase1 <- nls( density ~ Asym/(1 + exp((xmid - log(conc))/scal)),
                  data = DNase1,
                  start = list(Asym = 3, xmid = 0, scal = 1),
                  trace = TRUE, algorithm = "port")
summary(fm4DNase1)

## weighted nonlinear regression
Treated <- Puromycin[Puromycin$state == "treated", ]
weighted.MM <- function(resp, conc, Vm, K)
{
    ## Purpose: exactly as white book p.451 -- RHS for nls()
    ##  Weighted version of Michaelis-Menten model
    ## ------------------------------------------------------------
    ## Arguments: 'y', 'x' and the two parameters (see book)
    ## ------------------------------------------------------------
    ## Author: Martin Maechler, Date: 23 Mar 2001, 18:48

    pred <- (Vm * conc)/(K + conc)
    (resp - pred) / sqrt(pred)
}

Pur.wt <- nls( ~ weighted.MM(rate, conc, Vm, K), data = Treated,
              start = list(Vm = 200, K = 0.1),
              trace = TRUE)

## The two examples below show that you can fit a model to
## artificial data with noise but not to artificial data
## without noise.
x = 1:10
y = x                                  # perfect fit
yeps = y + rnorm(length(y), sd = 0.01) # added noise
nls(yeps ~ a + b*x, start = list(a = 0.12345, b = 0.54321),
     trace = TRUE)
\dontrun{
nls(y ~ a + b*x, start = list(a = 0.12345, b = 0.54321),
     trace = TRUE)
}       
}
\keyword{nonlinear}
\keyword{regression}
\keyword{models}
