\name{complex}
\title{Complex Vectors}
\usage{
complex(n = 0, real = numeric(), imaginary = numeric())
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
\alias{is.complex}
\alias{Re}
\alias{Im}
\alias{Mod}
\alias{Arg}
\alias{Conj}
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
}
\keyword{complex}
