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
\description{Bessel Functions of integer and fractional order, of first
    and second kind, \eqn{J_{\nu}}{J(nu)} and \eqn{Y_{\nu}}{Y(nu)}, and
    Modified Bessel functions (of first and third kind),
    \eqn{I_{\nu}}{I(nu)} and \eqn{K_{\nu}}{K(nu)}. 

    \code{gammaCody} is the \eqn{(\Gamma)} Function as from the Specfun
    package and originally used in the Bessel code.
}
\arguments{
    \item{x}{numeric, \eqn{\ge 0}{>= 0}.}
    \item{nu}{numeric; \eqn{\ge 0}{>= 0} unless in \code{besselK} which
	is symmetric in \code{nu}.  The \emph{order} of the
      corresponding Bessel function.}
    \item{expon.scaled}{logical; if \code{TRUE}, the results are
      exponentially scaled in order to avoid overflow
      (\eqn{I_{\nu}}{I(nu)}) or underflow (\eqn{K_{\nu}}{K(nu)}),
      respectively.}
}
\value{
    numeric of the same length of \code{x} with the (scaled, if
    \code{expon.scale=T}) values of the corresponding Bessel function.
}
\details{
    The underlying code for these functions stem from \emph{Netlib},
    \file{http://www.netlib.org/specfun/r[ijky]besl}.

    If \code{expon.scaled = TRUE}, \eqn{e^{-x} I_{\nu}(x)}{exp(-x) I(x;nu)},
    or \eqn{e^{x} K_{\nu}(x)}{exp(x) K(x;nu)} are returned.

    \code{gammaCody} may be somewhat faster but less
    precise and/or robust than \R's standard \code{\link{gamma}}.
    It is here for experimental purpose mainly, and \emph{may be defunct
      	very soon}.
}
\references{
  Abramowitz, M. and Stegun, I. A. (1972).\cr
  \emph{Handbook of Mathematical Functions}, Dover, New York;\cr
  Chapter 9: Bessel Functions of Integer Order.
}
\seealso{Other special mathematical functions, as the
    \code{\link{gamma}}, \eqn{\Gamma(x)}, and \code{\link{beta}},
    \eqn{B(x)}.
}
\author{Original Fortran code:
    W. J. Cody, Argonne National Laboratory \cr
    Translation to C and adaption to \R:
    Martin Maechler \email{maechler@stat.math.ethz.ch.}
\examples{
nus <- c(0:5,10,20)

x <- seq(0,4, len= 501)
plot(x,x, ylim = c(0,6), ylab="",type='n', main = "Bessel Functions  I_nu(x)")
for(nu in nus) lines(x,besselI(x,nu=nu), col = nu+2)
legend(0,6, leg=paste("nu=",nus), col = nus+2, lwd=1)

x <- seq(0,40,len=801); yl <- c(-.8,.8)
plot(x,x, ylim = yl, ylab="",type='n', main = "Bessel Functions  J_nu(x)")
for(nu in nus) lines(x,besselJ(x,nu=nu), col = nu+2)
legend(32,-.18, leg=paste("nu=",nus), col = nus+2, lwd=1)

x0 <- 2^(-20:10)
plot(x0,x0^-8, log='xy', ylab="",type='n',
     main = "Bessel Functions  J_nu(x)  near 0\n log - log  scale")
for(nu in sort(c(nus,nus+.5))) lines(x0,besselJ(x0,nu=nu), col = nu+2)
legend(3,1e50, leg=paste("nu=", paste(nus,nus+.5, sep=",")), col=nus+2, lwd=1)

plot(x0,x0^-8, log='xy', ylab="",type='n',
     main = "Bessel Functions  K_nu(x)  near 0\n log - log  scale")
for(nu in sort(c(nus,nus+.5))) lines(x0,besselK(x0,nu=nu), col = nu+2)
legend(3,1e50, leg=paste("nu=", paste(nus,nus+.5, sep=",")), col=nus+2, lwd=1)

x <- x[x > 0]
plot(x,x, ylim=c(1e-18,1e11),log="y", ylab="",type='n',
     main = "Bessel Functions  K_nu(x)")
for(nu in nus) lines(x,besselK(x,nu=nu), col = nu+2)
legend(0,1e-5, leg=paste("nu=",nus), col = nus+2, lwd=1)

## Check the Scaling :
for(nu in nus)
   print(all(abs(1- besselK(x,nu)*exp( x) / besselK(x,nu,expo=TRUE)) < 2e-15))
for(nu in nus)
   print(all(abs(1- besselI(x,nu)*exp(-x) / besselI(x,nu,expo=TRUE)) < 1e-15))

yl <- c(-1.6, .6)
plot(x,x, ylim = yl, ylab="",type='n', main = "Bessel Functions  Y_nu(x)")
for(nu in nus){xx <- x[x > .6*nu]; lines(xx,besselY(xx,nu=nu), col = nu+2)}
legend(25,-.5, leg=paste("nu=",nus), col = nus+2, lwd=1)

\testonly{
 x0 <- 2^(-20:10)
 plot(x0,x0, log='xy', ylab="", ylim=c(.1,1e60),type='n',
      main = "Bessel Functions -Y_nu(x)  near 0\n log - log  scale")
 for(nu in sort(c(nus,nus+.5))) lines(x0, -besselY(x0,nu=nu), col = nu+2)
 legend(3,1e50, leg=paste("nu=", paste(nus,nus+.5, sep=",")), col=nus+2, lwd=1)

 x <- seq(3,500);yl <- c(-.3, .2)
 plot(x,x, ylim = yl, ylab="",type='n', main = "Bessel Functions  Y_nu(x)")
 for(nu in nus){xx <- x[x > .6*nu]; lines(xx,besselY(xx,nu=nu), col = nu+2)}
 legend(300,-.08, leg=paste("nu=",nus), col = nus+2, lwd=1)

 x <- seq(10,50000,by=10);yl <- c(-.1, .1)
 plot(x,x, ylim = yl, ylab="",type='n', main = "Bessel Functions  Y_nu(x)")
 for(nu in nus){xx <- x[x > .6*nu]; lines(xx,besselY(xx,nu=nu), col = nu+2)}
 summary(bY <- besselY(2,nu = nu <- seq(0,100,len=501)))
 which(bY >= 0)
 summary(bY <- besselY(2,nu = nu <- seq(3,300,len=51)))
 summary(bI <- besselI(x = x <- 10:700, 1))
}

}
\keyword{math}

