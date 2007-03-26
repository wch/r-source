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
%\alias{vcov.nls}
%\alias{weights.nls}
\title{Nonlinear Least Squares}
\description{
  Determine the nonlinear (weighted) least-squares estimates of the
  parameters of a nonlinear model.
}
\usage{
nls(formula, data, start, control, algorithm,
    trace, subset, weights, na.action, model,
    lower, upper, \dots)
}
\arguments{
  \item{formula}{a nonlinear model \link{formula} including variables and
    parameters.  Will be coerced to a formula if necessary.}
  \item{data}{an optional data frame in which to evaluate the variables in
    \code{formula}.  Can also be a list or an environment, but not a matrix.}
  \item{start}{a named list or named numeric vector of starting
    estimates.  Since R 2.4.0, when \code{start} is missing, a very
    cheap guess for \code{start} is tried (if \code{algorithm != "plinear"}).
  }
  \item{control}{an optional list of control settings.  See
    \code{\link{nls.control}} for the names of the settable control
    values and their effect.}
  \item{algorithm}{character string specifying the algorithm to use.
    The default algorithm is a Gauss-Newton algorithm.  Other possible
    values are \code{"plinear"} for the Golub-Pereyra algorithm for
    partially linear least-squares models and \code{"port"} for the
    \sQuote{nl2sol} algorithm from the Port package.}
  \item{trace}{logical value indicating if a trace of the iteration
    progress should be printed.  Default is \code{FALSE}.  If
    \code{TRUE} the residual (weighted) sum-of-squares and the
    parameter values are printed at the conclusion of each iteration.
    When the \code{"plinear"} algorithm is used, the conditional
    estimates of the linear parameters are printed after the nonlinear
    parameters.  When the \code{"port"} algorithm is used the
    objective function value printed is half the residual (weighted)
    sum-of-squares.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used in the fitting process.}
  \item{weights}{an optional numeric vector of (fixed) weights.  When
    present, the objective function is weighted least squares.}
  \item{na.action}{a function which indicates what should happen
    when the data contain \code{NA}s.  The default is set by
    the \code{na.action} setting of \code{\link{options}}, and is
    \code{\link{na.fail}} if that is unset.  The \dQuote{factory-fresh}
    default is \code{\link{na.omit}}.  Value \code{\link{na.exclude}}
    can be useful.}
  \item{model}{logical.  If true, the model frame is returned as part of
    the object. Default is \code{FALSE}.}
  \item{lower, upper}{vectors of lower and upper bounds, replicated to
    be as long as \code{start}.  If unspecified, all parameters are
    assumed to be unconstrained.  Bounds can only be used with the
    \code{"port"} algorithm.  They are ignored, with a warning, if given
    for other algorithms.}
  \item{\dots}{Additional optional arguments.  None are used at present.}
}
\details{
  An \code{nls} object is a type of fitted model object.  It has methods
  for the generic functions \code{\link{anova}}, \code{\link{coef}},
  \code{\link{confint}}, \code{\link{deviance}},
  \code{\link{df.residual}}, \code{\link{fitted}},
  \code{\link{formula}}, \code{\link{logLik}}, \code{\link{predict}},
  \code{\link{print}}, \code{\link{profile}}, \code{\link{residuals}},
  \code{\link{summary}}, \code{\link{vcov}} and \code{\link{weights}}.

  Variables in \code{formula} are looked for first in \code{data}, then
  the environment of \code{formula} and finally along the search path.
  Functions in \code{formula} are searched for first in the environment
  of \code{formula} and then along the search path.
}
\section{Warning}{
  \bold{Do not use \code{nls} on artificial "zero-residual" data.}

  The \code{nls} function uses a relative-offset convergence criterion
  that compares the numerical imprecision at the current parameter
  estimates to the residual sum-of-squares.  This performs well on data of
  the form \deqn{y=f(x,\theta)+\epsilon}{y = f(x, theta) + eps} (with
  \code{var(eps) > 0}).  It fails to indicate convergence on data of the form
  \deqn{y=f(x,\theta)}{y = f(x, theta)} because the criterion amounts to
  comparing two components of the round-off error.  If you wish to test
  \code{nls} on artificial data please add a noise component, as shown
  in the example below.

  The \code{algorithm = "port"} code appears unfinished, and does
  not even check that the starting value is within the bounds.
  Use with caution, especially where bounds are supplied.
}
\value{
  A list of
  \item{m}{an \code{nlsModel} object incorporating the model.}
  \item{data}{the expression that was passed to \code{nls} as the data
    argument.  The actual data values are present in the environment of
    the \code{m} component.}
  \item{call}{the matched call with several components, notably
    \code{algorithm}.}
  \item{na.action}{the \code{"na.action"} attribute (if any) of the
    model frame.}
  \item{dataClasses}{the \code{"dataClasses"} attribute (if any) of the
    \code{"terms"} attribute of the model frame.}
  \item{model}{if \code{model = TRUE}, the model frame.}
  \item{weights}{if \code{weights} is supplied, the weights.}
  \item{convInfo}{when \code{algorithm} is not "port", a list with
    convergence information.}
  \item{control}{the control \code{list} used, see the \code{control}
    argument.}
  \item{convergence, message}{for an \code{algorithm = "port"} fit only,
    a convergence code (\code{0} for convergence) and message.
  }

  \bold{Note} that setting \code{warnOnly = TRUE} in the \code{control}
  argument (see \code{\link{nls.control}}) returns a non-converged
  object (since \R version 2.5.0) which might be useful for further
  convergence analysis, \emph{but \bold{not} for inference}.
}
\references{
  Bates, D. M. and Watts, D. G. (1988)
  \emph{Nonlinear Regression Analysis and Its Applications},
  Wiley

  Bates, D. M. and Chambers, J. M. (1992)
  \emph{Nonlinear models.}
  Chapter 10 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth \& Brooks/Cole.

  \url{http://netlib.bell-labs.com/netlib/port/} for the Port library
  documentation.
}
\author{Douglas M. Bates and Saikat DebRoy}
\seealso{
  \code{\link{summary.nls}}, \code{\link{predict.nls}},
  \code{\link{profile.nls}}.
}
\examples{
DNase1 <- subset(DNase, Run == 1)

## using a selfStart model
fm1DNase1 <- nls(density ~ SSlogis(log(conc), Asym, xmid, scal), DNase1)
summary(fm1DNase1)
## the coefficients only:
coef(fm1DNase1)
## including their SE, etc:
coef(summary(fm1DNase1))

## using conditional linearity
fm2DNase1 <- nls(density ~ 1/(1 + exp((xmid - log(conc))/scal)),
                 data = DNase1,
                 start = list(xmid = 0, scal = 1),
                 alg = "plinear", trace = TRUE)
summary(fm2DNase1)

## without conditional linearity
fm3DNase1 <- nls(density ~ Asym/(1 + exp((xmid - log(conc))/scal)),
                 data = DNase1,
                 start = list(Asym = 3, xmid = 0, scal = 1),
                 trace = TRUE)
summary(fm3DNase1)

## using Port's nl2sol algorithm
fm4DNase1 <- nls(density ~ Asym/(1 + exp((xmid - log(conc))/scal)),
                 data = DNase1,
                 start = list(Asym = 3, xmid = 0, scal = 1),
                 trace = TRUE, algorithm = "port")
summary(fm4DNase1)

## weighted nonlinear regression
Treated <- Puromycin[Puromycin$state == "treated", ]
weighted.MM <- function(resp, conc, Vm, K)
{
    ## Purpose: exactly as white book p. 451 -- RHS for nls()
    ##  Weighted version of Michaelis-Menten model
    ## ----------------------------------------------------------
    ## Arguments: 'y', 'x' and the two parameters (see book)
    ## ----------------------------------------------------------
    ## Author: Martin Maechler, Date: 23 Mar 2001

    pred <- (Vm * conc)/(K + conc)
    (resp - pred) / sqrt(pred)
}

Pur.wt <- nls( ~ weighted.MM(rate, conc, Vm, K), data = Treated,
              start = list(Vm = 200, K = 0.1),
              trace = TRUE)
summary(Pur.wt)

## The two examples below show that you can fit a model to
## artificial data with noise but not to artificial data
## without noise.
x <- 1:10
y <- 2*x + 3                            # perfect fit
yeps <- y + rnorm(length(y), sd = 0.01) # added noise
nls(yeps ~ a + b*x, start = list(a = 0.12345, b = 0.54321),
    trace = TRUE)
\dontrun{
## terminates in an error, because convergence cannot be confirmed:
nls(y ~ a + b*x, start = list(a = 0.12345, b = 0.54321),
     trace = TRUE)
}

## nls() internal very cheap guess for starting values can be sufficient:

x <- -(1:100)/10
y <- 100 + 10 * exp(x / 2) + rnorm(x)/10
nlmod <- nls(y ~  Const + A * exp(B * x), trace=TRUE)

plot(x,y, main = "nls(*), data, true function and fit, n=100")
curve(100 + 10 * exp(x / 2), col=4, add = TRUE)
lines(x, predict(nlmod), col=2)
}
\keyword{nonlinear}
\keyword{regression}
\keyword{models}
