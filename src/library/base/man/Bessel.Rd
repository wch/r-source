\name{Bessel}
\title{Bessel Functions}
\alias{besselI}
\alias{besselJ}
\alias{besselK}
\alias{besselY}
\usage{
}
\description{Bessel Functions of integer and fractional order, of first
    and second kind, \eqn{J_{\nu}}{J(nu)} and \eqn{Y_{\nu}}{Y(nu)}, and
    Modified Bessel functions \eqn{I_{\nu}}{I(nu)} and \eqn{K_{\nu}}{K(nu)}.
}
\arguments{
    \item{x}{numeric}
    \item{nu}{numeric}
\value{

}
\details{
}
\references{
  Abramowitz, M. and Stegun, I. A. (1972).\cr
  \emph{Handbook of Mathematical Functions}, Dover, New York;\cr
  Chapter 9: Bessel Functions of Integer Order.
}
\seealso{Other special mathematical functions, as the
    \code{\link{gamma}}, \eqn{\Gamma(x)}, and  \code{\link{beta}},
    \eqn{\Beta(x)}.
}
\author{Translation to C and adaption to \R: Martin Maechler.}
\examples{
x <- seq(0,4, len= 501)
nus <- c(0:5,10,20)

plot(x,x, ylim = c(0,6), ylab="",type='n', main = "Bessel Functions  I_nu(x)")
for(nu in nus) lines(x,besselI(x,nu=nu), col = nu+2)
legend(0,6, leg=paste("nu=",nus), col = nus+2, lwd=1)

x <- seq(0,20,len=501); yl <- c(-.8,.8)
plot(x,x, ylim = yl, ylab="",type='n', main = "Bessel Functions  J_nu(x)")
for(nu in nus) lines(x,besselJ(x,nu=nu), col = nu+2)
legend(0,-.1, leg=paste("nu=",nus), col = nus+2, lwd=1)

x <- seq(0,20,len=501); yl <- c(-.8,.8)
plot(x,x, ylim = c(0,6), ylab="",type='n', main = "Bessel Functions  K_nu(x)")
for(nu in nus) lines(x,besselK(x,nu=nu), col = nu+2)
legend(0,6, leg=paste("nu=",nus), col = nus+2, lwd=1)

plot(x,x, ylim = c(0,6), ylab="",type='n', main = "Bessel Functions  Y_nu(x)")
for(nu in nus) lines(x,besselY(x,nu=nu), col = nu+2)
legend(0,6, leg=paste("nu=",nus), col = nus+2, lwd=1)

}
\keyword{math}

