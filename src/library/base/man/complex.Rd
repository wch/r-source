\name{complex}
\title{Complex Vectors}
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
  Basic functions which support complex arithmetic in R.
}
\usage{
complex(length.out = 0, real = numeric(), imaginary = numeric(),
        modulus = 1, argument = 0)
as.complex(x, \dots)
is.complex(x)

Re(x)
Im(x)
Mod(x)
Arg(x)
Conj(x)
}
\arguments{
  \item{length.out}{numeric.  Desired length of the output vector,
    inputs being recycled as needed.}
  \item{real}{numeric vector.}
  \item{imaginary}{numeric vector.}
  \item{modulus}{numeric vector.}
  \item{argument}{numeric vector.}
  \item{x}{an object, probably of mode \code{complex}.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  Complex vectors can be created with \code{complex}.  The vector can be
  specified either by giving its length, its real and imaginary parts, or
  modulus and argument.  (Giving just the length generates a vector of
  complex zeroes.)

  \code{as.complex} attempts to coerce its argument to be of complex
  type: like \code{\link{as.vector}} it strips attributes including names.

  Note that \code{is.complex} and \code{is.numeric} are never both
  \code{TRUE}.

  The functions \code{Re}, \code{Im}, \code{Mod}, \code{Arg} and
  \code{Conj} have their usual interpretation as returning the real
  part, imaginary part, modulus, argument and complex conjugate for
  complex values.  Modulus and argument are also called the \emph{polar
    coordinates}.  If \eqn{z = x + i y} with real \eqn{x} and \eqn{y},
  for \eqn{r = \code{Mod}(z) = \sqrt{x^2 + y^2}}, and
  \eqn{\phi = \code{Arg}(z)}, \eqn{x = r*\cos(\phi)} and \eqn{y = r*\sin(\phi)}.
  They are all generic functions: methods can be defined
  for them individually or via the \code{\link{Complex}} group generic.

  In addition, the elementary trigonometric, logarithmic and exponential
  functions are available for complex values.

  \code{is.complex} is generic: you can write methods to handle
  specific classes of objects, see \link{InternalMethods}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\examples{
0i ^ (-3:3)

matrix(1i^ (-6:5), nr=4)#- all columns are the same
0 ^ 1i # a complex NaN

## create a complex normal vector
z <- complex(real = rnorm(100), imag = rnorm(100))
## or also (less efficiently):
z2 <- 1:2 + 1i*(8:9)

## The Arg(.) is an angle:
zz <- (rep(1:4,len=9) + 1i*(9:1))/10
zz.shift <- complex(modulus = Mod(zz), argument= Arg(zz) + pi)
plot(zz, xlim=c(-1,1), ylim=c(-1,1), col="red", asp = 1,
     main = expression(paste("Rotation by "," ", pi == 180^o)))
abline(h=0,v=0, col="blue", lty=3)
points(zz.shift, col="orange")
}
\keyword{complex}
