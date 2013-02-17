% File src/library/base/man/Bessel.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{Bessel}
\title{Bessel Functions}
\alias{bessel}
\alias{Bessel}
\alias{besselI}
\alias{besselJ}
\alias{besselK}
\alias{besselY}
\usage{
besselI(x, nu, expon.scaled = FALSE)
besselK(x, nu, expon.scaled = FALSE)
besselJ(x, nu)
besselY(x, nu)
}
\description{
  Bessel Functions of integer and fractional order, of first
  and second kind, \eqn{J_{\nu}}{J(nu)} and \eqn{Y_{\nu}}{Y(nu)}, and
  Modified Bessel functions (of first and third kind),
  \eqn{I_{\nu}}{I(nu)} and \eqn{K_{\nu}}{K(nu)}.
}
\arguments{
  \item{x}{numeric, \eqn{\ge 0}.}

  \item{nu}{numeric; The \emph{order} (maybe fractional!) of the
    corresponding Bessel function.}

  \item{expon.scaled}{logical; if \code{TRUE}, the results are
    exponentially scaled in order to avoid overflow
    (\eqn{I_{\nu}}{I(nu)}) or underflow (\eqn{K_{\nu}}{K(nu)}),
    respectively.}
}
\value{
  Numeric vector with the (scaled, if \code{expon.scaled = TRUE})
  values of the corresponding Bessel function.
  
  The length of the result is the maximum of the lengths of the
  parameters.  All parameters are recycled to that length.
}
\details{
  If \code{expon.scaled = TRUE}, \eqn{e^{-x} I_{\nu}(x)}{exp(-x) I(x;nu)},
  or \eqn{e^{x} K_{\nu}(x)}{exp(x) K(x;nu)} are returned.

  For \eqn{\nu < 0}{nu < 0}, formulae 9.1.2 and 9.6.2 from Abramowitz &
  Stegun  are applied (which is probably suboptimal), except for
  \code{besselK} which is symmetric in \code{nu}.
}
\source{
  The C code is a translation of Fortran routines from
  \url{http://www.netlib.org/specfun/ribesl}, \samp{../rjbesl}, etc.
}
\references{
  Abramowitz, M. and Stegun, I. A. (1972)
  \emph{Handbook of Mathematical Functions.} Dover, New York;
  Chapter 9: Bessel Functions of Integer Order.
}
\seealso{
  Other special mathematical functions, such as
  \code{\link{gamma}}, \eqn{\Gamma(x)}, and \code{\link{beta}},
  \eqn{B(x)}.
}
\author{
  Original Fortran code:
  W. J. Cody, Argonne National Laboratory \cr
  Translation to C and adaption to \R:
  Martin Maechler \email{maechler@stat.math.ethz.ch}.
}
\examples{
require(graphics)

nus <- c(0:5, 10, 20)

x <- seq(0, 4, length.out = 501)
plot(x, x, ylim = c(0, 6), ylab = "", type = "n",
     main = "Bessel Functions  I_nu(x)")
for(nu in nus) lines(x, besselI(x, nu = nu), col = nu + 2)
legend(0, 6, legend = paste("nu=", nus), col = nus + 2, lwd = 1)

x <- seq(0, 40, length.out = 801); yl <- c(-.8, .8)
plot(x, x, ylim = yl, ylab = "", type = "n",
     main = "Bessel Functions  J_nu(x)")
for(nu in nus) lines(x, besselJ(x, nu = nu), col = nu + 2)
legend(32, -.18, legend = paste("nu=", nus), col = nus + 2, lwd = 1)

## Negative nu's :
xx <- 2:7
nu <- seq(-10, 9, length.out = 2001)
op <- par(lab = c(16, 5, 7))
matplot(nu, t(outer(xx, nu, besselI)), type = "l", ylim = c(-50, 200),
        main = expression(paste("Bessel ", I[nu](x), " for fixed ", x,
                                ",  as ", f(nu))),
        xlab = expression(nu))
abline(v = 0, col = "light gray", lty = 3)
legend(5, 200, legend = paste("x=", xx), col=seq(xx), lty=seq(xx))
par(op)

x0 <- 2^(-20:10)
plot(x0, x0^-8, log = "xy", ylab = "", type = "n",
     main = "Bessel Functions  J_nu(x)  near 0\n log - log  scale")
for(nu in sort(c(nus, nus+0.5)))
    lines(x0, besselJ(x0, nu = nu), col = nu + 2)
legend(3, 1e50, legend = paste("nu=", paste(nus, nus+0.5, sep=",")),
       col = nus + 2, lwd = 1)

plot(x0, x0^-8, log = "xy", ylab = "", type = "n",
     main = "Bessel Functions  K_nu(x)  near 0\n log - log  scale")
for(nu in sort(c(nus, nus+0.5)))
    lines(x0, besselK(x0, nu = nu), col = nu + 2)
legend(3, 1e50, legend = paste("nu=", paste(nus, nus + 0.5, sep = ",")),
       col = nus + 2, lwd = 1)

x <- x[x > 0]
plot(x, x, ylim = c(1e-18, 1e11), log = "y", ylab = "", type = "n",
     main = "Bessel Functions  K_nu(x)")
for(nu in nus) lines(x, besselK(x, nu = nu), col = nu + 2)
legend(0, 1e-5, legend=paste("nu=", nus), col = nus + 2, lwd = 1)

yl <- c(-1.6, .6)
plot(x, x, ylim = yl, ylab = "", type = "n",
     main = "Bessel Functions  Y_nu(x)")
for(nu in nus){
    xx <- x[x > .6*nu]
    lines(xx, besselY(xx, nu=nu), col = nu+2)
}
legend(25, -.5, legend = paste("nu=", nus), col = nus+2, lwd = 1)

## negative nu in bessel_Y -- was bogus for a long time
curve(besselY(x, -0.1), 0, 10, ylim = c(-3,1), ylab = "")
for(nu in c(seq(-0.2, -2, by = -0.1)))
  curve(besselY(x, nu), add = TRUE)
title(expression(besselY(x, nu) * "   " *
                 {nu == list(-0.1, -0.2, ..., -2)}))
}
\keyword{math}

