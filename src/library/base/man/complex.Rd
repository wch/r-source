\name{complex}
\title{Complex Vectors}
\alias{complex}
\alias{as.complex}
\alias{is.complex}
\alias{Re}
\alias{Im}
\alias{Mod}
\alias{Arg}
\alias{Conj}
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
\description{
  These are basic functions which support complex arithmetic in R.
  Complex vectors can be created with \code{complex}.  The vector can be
  specified either by giving its length, or its real and imaginary parts
  (or both).

  Note that \code{is.complex} and \code{is.numeric} are never both
  \code{TRUE}.

  The functions \code{Re}, \code{Im}, \code{Mod}, \code{Arg} and
  \code{Conj} have their usual interpretation as returning the real
  part, imaginary part, modulus, argument and complex conjugate for
  complex values.  In addition, the elementary trigonometric,
  logarithmic and exponential functions are available for complex
  values.
}
\examples{
## create a complex normal vector
z <- complex(real = rnorm(100), imag = rnorm(100))
## or also (less efficiently):
z2 <- 1:2 + 1i*(8:9)

zz <- (rep(1:4,len=9) + 1i*(9:1))/10
zz.shift <- complex(modulus = Mod(zz), argument= Arg(zz) + pi)
plot(zz, xlim=c(-1,1), ylim=c(-1,1), col="red", asp = 1,
     main = expression(paste("Rotation by "," ", pi == 180^o)))
abline(h=0,v=0, col="blue", lty=3)
points(zz.shift, col="orange")
}
\keyword{complex}
