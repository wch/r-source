% File src/library/stats/man/numericDeriv.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2020 R Core Team
% Distributed under GPL 2 or later

\name{numericDeriv}
\alias{numericDeriv}
\title{Evaluate Derivatives Numerically}
\description{
  \code{numericDeriv} numerically evaluates the gradient of an expression.
}
\usage{
numericDeriv(expr, theta, rho = parent.frame(), dir = 1,
             eps = .Machine$double.eps ^ (1/if(central) 3 else 2), central = FALSE)
}
\arguments{
  \item{expr}{\code{\link{expression}} or \code{\link{call}} to be
    differentiated.  Should evaluate to a \code{\link{numeric}} vector.}
  \item{theta}{\code{\link{character}} vector of names of numeric variables
    used in \code{expr}.}
  \item{rho}{\code{\link{environment}} containing all the variables needed to
    evaluate \code{expr}.}
  \item{dir}{numeric vector of directions, typically with values in
    \code{-1, 1} to use for the finite differences;
    will be recycled to the length of \code{theta}.}
  \item{eps}{a positive number, to be used as unit step size \eqn{h} for
    the approximate numerical derivative \eqn{ (f(x+h)-f(x))/h } or the
    central version, see \code{central}.}
  \item{central}{logical indicating if \emph{central} divided differences
    should be computed, i.e., \eqn{ (f(x+h) - f(x-h)) / 2h }.  These are
    typically more accurate but need more evaluations of \eqn{f()}.}
}
\details{
  This is a front end to the C function \code{numeric_deriv}, which is
  described in \emph{Writing R Extensions}.

  The numeric variables must be of type \code{double} and not \code{integer}.
  %% checked in C code .. why not just coerce them?   (TODO?)
}
\value{
  The value of \code{eval(expr, envir = rho)} plus a matrix
  attribute \code{"gradient"}.  The columns of this matrix are
  the derivatives of the value with respect to the variables listed in
  \code{theta}.
}
\author{Saikat DebRoy \email{saikat@stat.wisc.edu};
  tweaks and \code{eps}, \code{central} options by R Core Team.}
\examples{
myenv <- new.env()
myenv$mean <- 0.
myenv$sd   <- 1.
myenv$x    <- seq(-3., 3., length.out = 31)
nD <- numericDeriv(quote(pnorm(x, mean, sd)), c("mean", "sd"), myenv)
str(nD)

## Visualize :
require(graphics)
matplot(myenv$x, cbind(c(nD), attr(nD, "gradient")), type="l")
abline(h=0, lty=3)
## "gradient" is close to the true derivatives, you don't see any diff.:
curve( - dnorm(x), col=2, lty=3, lwd=2, add=TRUE)
curve(-x*dnorm(x), col=3, lty=3, lwd=2, add=TRUE)
##
\dontdiff{# shows 1.609e-8 on most platforms
all.equal(attr(nD,"gradient"),
          with(myenv, cbind(-dnorm(x), -x*dnorm(x))))
}
}
\keyword{models}

