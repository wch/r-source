\name{complex}
\title{Complex Vectors}
\usage{
complex(length.out = 0, real = numeric(), imaginary = numeric(),
        modulus = 1, argument = 0)
as.complex(z)
is.complex(z)

Re(z)
Im(z)
Mod(z)
Arg(z)
Conj(z)
}
\alias{complex}
\alias{as.complex}
\alias{as.complex.default}
\alias{is.complex}
\alias{Re}
\alias{Im}
\alias{Mod}
\alias{Arg}
\alias{Conj}
\description{
  These are basic functions which support complex arithmetic in R.
  Complex vectors can be created with \code{complex}.  The vector can be
  specified either by giving its length, its real and imaginary parts, or
  modulus and argument.
}
\details{
  Note that \code{is.complex} and \code{is.numeric} are never both \code{TRUE}.

  The functions \code{Re}, \code{Im}, \code{Mod}, \code{Arg} and
  \code{Conj} have their usual interpretation as returning the real
  part, imaginary part, modulus, argument and complex conjugate for
  complex values. Modulus and argument are also called the \emph{polar
      coordinates}. If \eqn{z = x + i y} with real \eqn{x} and \eqn{y},
  \code{Mod}\eqn{(z) = \sqrt{x^2 + y^2}}, and for
  \eqn{\phi= Arg(z)}, \eqn{x = \cos(\phi)} and \eqn{y = \sin(\phi)}.

  In addition, the elementary trigonometric, logarithmic and exponential
  functions are available for complex values.
}
\examples{
( z <- 0i ^ (-3:3) )
stopifnot(Re(z) == 0 ^ (-3:3))
matrix(1i^ (-6:5), nr=4)#- all columns are the same
0 ^ 1i # a complex NaN

## create a complex normal vector
z <- complex(real = rnorm(100), imag = rnorm(100))
## or also (less efficiently):
z2 <- 1:2 + 1i*(8:9)

all(Mod ( 1 -  sin(z) / ( (exp(1i*z)-exp(-1i*z))/(2*1i) ))
    < 100*.Machine$double.eps)
## The Arg(.) is an angle:
zz <- (rep(1:4,len=9) + 1i*(9:1))/10
zz.shift <- complex(modulus = Mod(zz), argument= Arg(zz) + pi)
plot(zz, xlim=c(-1,1), ylim=c(-1,1), col="red", asp = 1,
     main = expression(paste("Rotation by "," ", pi == 180^o)))
abline(h=0,v=0, col="blue", lty=3)
points(zz.shift, col="orange")
}
\keyword{complex}
