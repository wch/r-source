% File src/library/base/man/complex.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2010 R Core Team
% Distributed under GPL 2 or later

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
\description{
  Basic functions which support complex arithmetic in R.
}
\usage{
complex(length.out = 0, real = numeric(), imaginary = numeric(),
        modulus = 1, argument = 0)
as.complex(x, \dots)
is.complex(x)

Re(z)
Im(z)
Mod(z)
Arg(z)
Conj(z)
}
\arguments{
  \item{length.out}{numeric.  Desired length of the output vector,
    inputs being recycled as needed.}
  \item{real}{numeric vector.}
  \item{imaginary}{numeric vector.}
  \item{modulus}{numeric vector.}
  \item{argument}{numeric vector.}
  \item{x}{an object, probably of mode \code{complex}.}
  \item{z}{an object of mode \code{complex}, or one of a class for which
    a methods has been defined.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  Complex vectors can be created with \code{complex}.  The vector can be
  specified either by giving its length, its real and imaginary parts, or
  modulus and argument.  (Giving just the length generates a vector of
  complex zeroes.)

  \code{as.complex} attempts to coerce its argument to be of complex
  type: like \code{\link{as.vector}} it strips attributes including
  names.  All forms of \code{NA} and \code{NaN} are coerced to a complex
  \code{NA}, for which both the real and imaginary parts are \code{NA}.

  Note that \code{is.complex} and \code{is.numeric} are never both
  \code{TRUE}.

  The functions \code{Re}, \code{Im}, \code{Mod}, \code{Arg} and
  \code{Conj} have their usual interpretation as returning the real
  part, imaginary part, modulus, argument and complex conjugate for
  complex values.  The modulus and argument are also called the \emph{polar
  coordinates}.  If \eqn{z = x + i y} with real \eqn{x} and \eqn{y}, for
  \eqn{r = Mod(z) = \sqrt{x^2 + y^2}}{r = Mod(z) = \sqrt(x^2 + y^2)},
  and \eqn{\phi = Arg(z)}, \eqn{x = r*\cos(\phi)}{x = r*cos(\phi)} and
  \eqn{y = r*\sin(\phi)}{y = r*sin(\phi)}.  They are all
  \link{internal generic} \link{primitive} functions: methods can be
  defined for them
  individually or \emph{via} the \code{\link[=S3groupGeneric]{Complex}}
  group generic.

  In addition, the elementary trigonometric, logarithmic, exponential,
  square root and hyperbolic functions are implemented for complex
  values.

  Internally, complex numbers are stored as a pair of \link{double}
  precision numbers, either or both of which can be \code{\link{NaN}} or
  plus or minus infinity.
}
\section{S4 methods}{
  \code{as.complex} is primitive and can have S4 methods set.

  \code{Re}, \code{Im}, \code{Mod}, \code{Arg} and \code{Conj}
  constitute the S4 group generic
  \code{\link[=S4groupGeneric]{Complex}} and so S4 methods can be
  set for them individually or via the group generic.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\examples{
require(graphics)

0i ^ (-3:3)

matrix(1i^ (-6:5), nrow = 4) #- all columns are the same
0 ^ 1i # a complex NaN

## create a complex normal vector
z <- complex(real = stats::rnorm(100), imaginary = stats::rnorm(100))
## or also (less efficiently):
z2 <- 1:2 + 1i*(8:9)

## The Arg(.) is an angle:
zz <- (rep(1:4, len = 9) + 1i*(9:1))/10
zz.shift <- complex(modulus = Mod(zz), argument = Arg(zz) + pi)
plot(zz, xlim = c(-1,1), ylim = c(-1,1), col = "red", asp = 1,
     main = expression(paste("Rotation by "," ", pi == 180^o)))
abline(h = 0, v = 0, col = "blue", lty = 3)
points(zz.shift, col = "orange")
}
\keyword{complex}
