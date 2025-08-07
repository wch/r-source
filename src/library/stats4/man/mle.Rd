% File src/library/stats4/man/mle.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{mle}
\alias{mle}
\title{Maximum Likelihood Estimation}
\description{
  Estimate parameters by the method of maximum likelihood.
}
\usage{
mle(minuslogl, start,
       optim = stats::optim,
       method = if(!useLim) "BFGS" else "L-BFGS-B",
       fixed = list(), nobs, lower, upper, \dots)
}
\arguments{
  \item{minuslogl}{Function to calculate negative log-likelihood.}
  \item{start}{Named list of vectors or single vector. Initial values
    for optimizer. By default taken from the default arguments of \code{minuslogl}}
  \item{optim}{Optimizer function. (Experimental)}
  \item{method}{Optimization method to use. See \code{\link{optim}}.}
  \item{fixed}{Named list of vectors or single vector.  Parameter values to keep fixed during
    optimization.}
  \item{nobs}{optional integer: the number of observations, to be used for
    e.g.\sspace{}computing \code{\link{BIC}}.}
  \item{lower, upper}{Named lists of vectors or single vectors. Bounds for \code{\link{optim}}, if relevant.}
  \item{\dots}{Further arguments to pass to \code{\link{optim}}.}
}
\details{
  The \code{optim} optimizer is used to find the minimum of the
  negative log-likelihood.  An approximate covariance matrix for the
  parameters is obtained by inverting the Hessian matrix at the optimum.
  By default,  \code{\link{optim}} from the \pkg{stats} package is used; other
  optimizers need to be plug-compatible, both with respect to arguments
  and return values.

  The function \code{minuslogl} should take one or several arguments,
  each of which can be a vector. The optimizer optimizes a function
  which takes a single vector argument, containing the
  concatenation of the arguments to \code{minuslogl}, removing any
  values that should be held fixed. This function internally unpacks the
  argument vector, inserts the fixed values and calls \code{minuslogl}.

  The vector arguments \code{start}, \code{fixed}, \code{upper}, and
  \code{lower}, can be given in both packed and unpacked form, either as
  a single vector or as a list of vectors. In the latter case, you only
  need to specify those list elements that are actually affected. For vector
  arguments, including those inside lists, use a default marker for
  those values that you don't want to set: \code{NA} for \code{fixed}
  and \code{start}, and \code{+Inf, -Inf} for \code{upper}, and
  \code{lower}.
}
\value{
  An object of class \code{\link{mle-class}}.
}
\note{
  Notice that the \code{mll} argument should calculate -log L (not -2 log L). It
  is for the user to ensure that the likelihood is correct, and that
  asymptotic likelihood inference is valid.
}
\seealso{
  \code{\link{mle-class}}
}
\examples{
## Avoid printing to unwarranted accuracy
od <- options(digits = 5)

## Simulated EC50 experiment with count data
x <- 0:10
y <- c(26, 17, 13, 12, 20, 5, 9, 8, 5, 4, 8)

## Easy one-dimensional MLE:
nLL <- function(lambda) -sum(stats::dpois(y, lambda, log = TRUE))
fit0 <- mle(nLL, start = list(lambda = 5), nobs = NROW(y))

## sanity check --- notice that "nobs" must be input
## (not guaranteed to be meaningful for any likelihood)
stopifnot(nobs(fit0) == length(y))


# For 1D, this is preferable:
fit1 <- mle(nLL, start = list(lambda = 5), nobs = NROW(y),
            method = "Brent", lower = 1, upper = 20)

## This needs a constrained parameter space: most methods will accept NA
ll <- function(ymax = 15, xhalf = 6) {
    if(ymax > 0 && xhalf > 0)
      -sum(stats::dpois(y, lambda = ymax/(1+x/xhalf), log = TRUE))
    else NA
}
(fit <- mle(ll, nobs = length(y)))
mle(ll, fixed = list(xhalf = 6))

## Alternative using bounds on optimization
ll2 <- function(ymax = 15, xhalf = 6)
    -sum(stats::dpois(y, lambda = ymax/(1+x/xhalf), log = TRUE))
mle(ll2, lower = rep(0, 2))

AIC(fit)
BIC(fit)

summary(fit)
logLik(fit)
vcov(fit)
plot(profile(fit), absVal = FALSE)
confint(fit)

## Use bounded optimization
## The lower bounds are really > 0,
## but we use >=0 to stress-test profiling
(fit2 <- mle(ll2, lower = c(0, 0)))
plot(profile(fit2), absVal = FALSE)

## A better parametrization:
ll3 <- function(lymax = log(15), lxhalf = log(6))
    -sum(stats::dpois(y, lambda = exp(lymax)/(1+x/exp(lxhalf)), log = TRUE))
(fit3 <- mle(ll3))
plot(profile(fit3), absVal = FALSE)
exp(confint(fit3))

# Regression tests for bounded cases (this was broken in R 3.x)
fit4 <- mle(ll, lower = c(0, 4)) # has max on boundary
confint(fit4)

## direct check that fixed= and constraints work together
mle(ll, lower = c(0, 4), fixed=list(ymax=23)) # has max on boundary

## Linear regression using MLE
x <- 1:10 
y <- c(0.48, 2.24, 2.22, 5.15, 4.64, 5.53, 7, 8.8, 7.67, 9.23)

LM_mll <- function(formula, data = environment(formula))
{
     y <- model.response(model.frame(formula, data))
     X <- model.matrix(formula, data)
     b0 <- numeric(NCOL(X))
     names(b0) <- colnames(X)
     function(b=b0, sigma=1)
         -sum(dnorm(y, X \%*\% b, sigma, log=TRUE))
}

mll <- LM_mll(y ~ x)

summary(lm(y~x)) # for comparison -- notice variance bias in MLE
summary(mle(mll, lower=c(-Inf,-Inf, 0.01)))
summary(mle(mll, lower=list(sigma = 0.01))) # alternative specification

confint(mle(mll, lower=list(sigma = 0.01)))
plot(profile(mle(mll, lower=list(sigma = 0.01))))

Binom_mll <- function(x, n)
{
    force(x); force(n) ## beware lazy evaluation
    function(p=.5) -dbinom(x, n, p, log=TRUE)
}

## Likelihood functions for different x.
## This code goes wrong, if force(x) is not used in Binom_mll:

curve(Binom_mll(0, 10)(p), xname="p", ylim=c(0, 10))
mll_list <- list(10)
for (x in 1:10)
    mll_list[[x]] <- Binom_mll(x, 10)
for (mll in mll_list)
    curve(mll(p), xname="p", add=TRUE)

mll <- Binom_mll(4,10)
mle(mll, lower = 1e-16, upper = 1-1e-16) # limits must be inside (0,1)

## Boundary case: This works, but fails if limits are set closer to 0 and 1  
mll <- Binom_mll(0, 10)
mle(mll, lower=.005, upper=.995)

\dontrun{
## We can use limits closer to the boundaries if we use the
## drop-in replacement optimr() from the optimx package.

mle(mll, lower = 1e-16, upper = 1-1e-16, optim=optimx::optimr)
}


options(od)
}
\keyword{models}

