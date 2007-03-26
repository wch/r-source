\name{Bessel}
\title{Bessel Functions}
\alias{bessel}
\alias{Bessel}
\alias{besselI}
\alias{besselJ}
\alias{besselK}
\alias{besselY}
\alias{gammaCody}
\usage{
besselI(x, nu, expon.scaled = FALSE)
besselK(x, nu, expon.scaled = FALSE)
besselJ(x, nu)
besselY(x, nu)
gammaCody(x)
}
\description{
  Bessel Functions of integer and fractional order, of first
  and second kind, \eqn{J_{\nu}}{J(nu)} and \eqn{Y_{\nu}}{Y(nu)}, and
  Modified Bessel functions (of first and third kind),
  \eqn{I_{\nu}}{I(nu)} and \eqn{K_{\nu}}{K(nu)}. 

  \code{gammaCody} is the \eqn{(\Gamma)} function as from the Specfun
  package and originally used in the Bessel code.
}
\arguments{
  \item{x}{numeric, \eqn{\ge 0}{>= 0}.}

  \item{nu}{numeric; The \emph{order} (maybe fractional!) of the
    corresponding Bessel function.}

  \item{expon.scaled}{logical; if \code{TRUE}, the results are
    exponentially scaled in order to avoid overflow
    (\eqn{I_{\nu}}{I(nu)}) or underflow (\eqn{K_{\nu}}{K(nu)}),
    respectively.}
}
\value{
  Numeric vector of the same length of \code{x} with the (scaled, if
  \code{expon.scale=TRUE}) values of the corresponding Bessel function.
}
\details{
  The underlying C code stems from \emph{Netlib}
  (\url{http://www.netlib.org/specfun/r[ijky]besl}).

  If \code{expon.scaled = TRUE}, \eqn{e^{-x} I_{\nu}(x)}{exp(-x) I(x;nu)},
  or \eqn{e^{x} K_{\nu}(x)}{exp(x) K(x;nu)} are returned.

  \code{gammaCody} may be somewhat faster but less precise and/or robust
  than \R's standard \code{\link{gamma}}.  It is here for experimental
  purpose mainly, and \emph{may be defunct very soon}.

  For \eqn{\nu < 0}{nu < 0}, formulae 9.1.2 and 9.6.2 from the
  reference below are applied (which is probably suboptimal), unless for
  \code{besselK} which is symmetric in \code{nu}.  
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
  Martin Maechler \email{maechler@stat.math.ethz.ch.}
}
\examples{
nus <- c(0:5, 10, 20)

x <- seq(0, 4, len = 501)
plot(x, x, ylim = c(0, 6), ylab = "", type = "n",
     main = "Bessel Functions  I_nu(x)")
for(nu in nus) lines(x, besselI(x, nu=nu), col = nu+2)
legend(0, 6, legend = paste("nu=", nus), col = nus+2, lwd = 1)

x <- seq(0, 40, len=801); yl <- c(-.8, .8)
plot(x, x, ylim = yl, ylab = "", type = "n",
     main = "Bessel Functions  J_nu(x)")
for(nu in nus) lines(x, besselJ(x, nu=nu), col = nu+2)
legend(32,-.18, legend = paste("nu=", nus), col = nus+2, lwd = 1)

## Negative nu's :
xx <- 2:7
nu <- seq(-10, 9, len = 2001)
op <- par(lab = c(16, 5, 7))
matplot(nu, t(outer(xx, nu, besselI)), type = "l", ylim = c(-50, 200),
        main = expression(paste("Bessel ", I[nu](x), " for fixed ", x,
                                ",  as ", f(nu))),
        xlab = expression(nu))
abline(v=0, col = "light gray", lty = 3)
legend(5, 200, legend = paste("x=", xx), col=seq(xx), lty=seq(xx))
par(op)

x0 <- 2^(-20:10)
plot(x0, x0^-8, log="xy", ylab="",type="n",
     main = "Bessel Functions  J_nu(x)  near 0\n log - log  scale")
for(nu in sort(c(nus, nus+.5)))
    lines(x0, besselJ(x0, nu=nu), col = nu+2)
legend(3, 1e50, legend = paste("nu=", paste(nus, nus+.5, sep=",")),
       col = nus + 2, lwd = 1)

plot(x0, x0^-8, log="xy", ylab="", type="n",
     main = "Bessel Functions  K_nu(x)  near 0\n log - log  scale")
for(nu in sort(c(nus, nus+.5)))
    lines(x0, besselK(x0, nu=nu), col = nu+2)
legend(3, 1e50, legend = paste("nu=", paste(nus, nus+.5, sep=",")),
       col = nus + 2, lwd = 1)

x <- x[x > 0]
plot(x, x, ylim=c(1e-18, 1e11), log = "y", ylab = "", type = "n",
     main = "Bessel Functions  K_nu(x)")
for(nu in nus) lines(x, besselK(x, nu=nu), col = nu+2)
legend(0, 1e-5, legend=paste("nu=", nus), col = nus+2, lwd = 1)

yl <- c(-1.6, .6)
plot(x, x, ylim = yl, ylab = "", type = "n",
     main = "Bessel Functions  Y_nu(x)")
for(nu in nus){
    xx <- x[x > .6*nu]
    lines(xx, besselY(xx, nu=nu), col = nu+2)
}
legend(25, -.5, legend = paste("nu=", nus), col = nus+2, lwd = 1)
}
\keyword{math}

