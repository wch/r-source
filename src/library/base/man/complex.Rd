% File src/library/base/man/complex.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{complex}
\title{Complex Numbers and Basic Functionality}
\alias{complex}
\alias{as.complex}
\alias{is.complex}
\alias{Re}
\alias{Im}
\alias{Mod}
\alias{Arg}
\alias{Conj}
\description{
  Basic functions which support complex arithmetic in \R, in addition to
  the arithmetic operators \code{+}, \code{-}, \code{*}, \code{/}, and \code{^}.
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
  names.  Up to \R versions 3.2.x, all forms of \code{NA} and \code{NaN}
  were coerced to a complex \code{NA}, i.e., the \code{\link{NA_complex_}}
  constant, for which both the real and imaginary parts are \code{NA}.
  Since \R 3.3.0, typically only objects which are \code{NA} in parts
  are coerced to complex \code{NA}, but others with \code{NaN} parts,
  are \emph{not}.  As a consequence, complex arithmetic where only
  \code{NaN}'s (but no \code{NA}'s) are involved typically will
  \emph{not} give complex \code{NA} but complex numbers with real or
  imaginary parts of \code{NaN}.

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

  In addition to the arithmetic operators (see \link{Arithmetic})
  \code{+}, \code{-}, \code{*}, \code{/}, and \code{^}, the elementary
  trigonometric, logarithmic, exponential, square root and hyperbolic
  functions are implemented for complex values.

  Matrix multiplications (\code{\link{\%*\%}}, \code{\link{crossprod}},
  \code{\link{tcrossprod}}) are also defined for complex matrices
  (\code{\link{matrix}}), and so are \code{\link{solve}},
  \code{\link{eigen}} or \code{\link{svd}}.

  Internally, complex numbers are stored as a pair of \link{double}
  precision numbers, either or both of which can be \code{\link{NaN}}
  (including \code{NA}, see \code{\link{NA_complex_}} and above) or
  plus or minus infinity.
}
\note{
  Operations and functions involving complex \code{\link{NaN}} mostly
  rely on the C library's handling of \samp{double complex} arithmetic,
  which typically returns \code{complex(re=NaN, im=NaN)} (but we have
  not seen a guarantee for that).
  For \code{+} and \code{-}, \R's own handling works strictly
  \dQuote{coordinate wise}.

  Operations involving complex \code{NA}, i.e., \code{\link{NA_complex_}}, return
  \code{\link{NA_complex_}}.
}
\seealso{
  \code{\link{Arithmetic}}; \code{\link{polyroot}} finds all \eqn{n}
  complex roots of a polynomial of degree \eqn{n}.
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

showC <- function(z) noquote(sprintf("(R = %g, I = %g)", Re(z), Im(z)))

## The exact result of this *depends* on the platform, compiler, math-library:
(NpNA <- NaN + NA_complex_) ; str(NpNA) # *behaves* as 'cplx NA' ..
stopifnot(is.na(NpNA), is.na(NA_complex_), is.na(Re(NA_complex_)), is.na(Im(NA_complex_)))
showC(NpNA)# but not always is {shows  '(R = NaN, I = NA)' on some platforms}
## and this is not TRUE everywhere:
identical(NpNA, NA_complex_)
showC(NA_complex_) # always == (R = NA, I = NA)
}
\keyword{complex}
