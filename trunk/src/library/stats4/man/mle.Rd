\name{mle}
\alias{mle}
\title{Maximum Likelihood Estimation}
\description{
  Estimate parameters by the method of maximum likelihood.
}
\usage{
mle(minuslogl, start = formals(minuslogl), method = "BFGS",
    fixed = list(), \dots)
}
\arguments{
  \item{minuslogl}{Function to calculate negative log-likelihood.}
  \item{start}{Named list. Initial values for optimizer.}
  \item{method}{Optimization method to use. See \code{\link{optim}}.}
  \item{fixed}{Named list.  Parameter values to keep fixed during
    optimization.}
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
x <- 0:10
y <- c(26, 17, 13, 12, 20, 5, 9, 8, 5, 4, 8)
ll <- function(ymax=15, xhalf=6)
    -sum(stats::dpois(y, lambda=ymax/(1+x/xhalf), log=TRUE))
(fit <- mle(ll))
mle(ll, fixed=list(xhalf=6))

summary(fit)
logLik(fit)
vcov(fit)
plot(profile(fit), absVal=FALSE)
confint(fit)

## use bounded optimization
## the lower bounds are really > 0, but we use >=0 to stress-test profiling
(fit1 <- mle(ll, method="L-BFGS-B", lower=c(0, 0)))
plot(profile(fit1), absVal=FALSE)

## a better parametrization:
ll2 <- function(lymax=log(15), lxhalf=log(6))
    -sum(stats::dpois(y, lambda=exp(lymax)/(1+x/exp(lxhalf)), log=TRUE))
(fit2 <- mle(ll2))
plot(profile(fit2), absVal=FALSE)
exp(confint(fit2))
}
\keyword{models}

