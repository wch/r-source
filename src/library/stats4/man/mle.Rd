% File src/library/stats4/man/mle.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{mle}
\alias{mle}
\title{Maximum Likelihood Estimation}
\description{
  Estimate parameters by the method of maximum likelihood.
}
\usage{
mle(minuslogl, start = formals(minuslogl), method = "BFGS",
    fixed = list(), nobs, \dots)
}
\arguments{
  \item{minuslogl}{Function to calculate negative log-likelihood.}
  \item{start}{Named list. Initial values for optimizer.}
  \item{method}{Optimization method to use. See \code{\link{optim}}.}
  \item{fixed}{Named list.  Parameter values to keep fixed during
    optimization.}
  \item{nobs}{optional integer: the number of observations, to be used for
    e.g.\sspace{}computing \code{\link{BIC}}.}
  \item{\dots}{Further arguments to pass to \code{\link{optim}}.}
}
\details{
  The \code{\link{optim}} optimizer is used to find the minimum of the
  negative log-likelihood.  An approximate covariance matrix for the
  parameters is obtained by inverting the Hessian matrix at the optimum.
}
\value{
  An object of class \code{\link{mle-class}}.
}
\note{
  Be careful to note that the argument is -log L (not -2 log L). It
  is for the user to ensure that the likelihood is correct, and that
  asymptotic likelihood inference is valid.
}
\seealso{
  \code{\link{mle-class}}
}
\examples{
## Avoid printing to unwarranted accuracy
od <- options(digits = 5)
x <- 0:10
y <- c(26, 17, 13, 12, 20, 5, 9, 8, 5, 4, 8)

## Easy one-dimensional MLE:
nLL <- function(lambda) -sum(stats::dpois(y, lambda, log = TRUE))
fit0 <- mle(nLL, start = list(lambda = 5), nobs = NROW(y))
# For 1D, this is preferable:
fit1 <- mle(nLL, start = list(lambda = 5), nobs = NROW(y),
            method = "Brent", lower = 1, upper = 20)
stopifnot(nobs(fit0) == length(y))

## This needs a constrained parameter space: most methods will accept NA
ll <- function(ymax = 15, xhalf = 6) {
    if(ymax > 0 && xhalf > 0)
      -sum(stats::dpois(y, lambda = ymax/(1+x/xhalf), log = TRUE))
    else NA
}
(fit <- mle(ll, nobs = length(y)))
mle(ll, fixed = list(xhalf = 6))
## alternative using bounds on optimization
ll2 <- function(ymax = 15, xhalf = 6)
    -sum(stats::dpois(y, lambda = ymax/(1+x/xhalf), log = TRUE))
mle(ll2, method = "L-BFGS-B", lower = rep(0, 2))

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
(fit2 <- mle(ll, method = "L-BFGS-B", lower = c(0, 0)))
plot(profile(fit2), absVal = FALSE)

## a better parametrization:
ll3 <- function(lymax = log(15), lxhalf = log(6))
    -sum(stats::dpois(y, lambda = exp(lymax)/(1+x/exp(lxhalf)), log = TRUE))
(fit3 <- mle(ll3))
plot(profile(fit3), absVal = FALSE)
exp(confint(fit3))

options(od)
}
\keyword{models}

