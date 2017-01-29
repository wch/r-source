% File src/library/base/man/Trig.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{Trig}
\title{Trigonometric Functions}
\alias{Trig}
\alias{cos}
\alias{sin}
\alias{tan}
\alias{acos}\alias{arccos}
\alias{asin}\alias{arcsin}
\alias{atan}\alias{arctan}
\alias{atan2}
\alias{cospi}
\alias{sinpi}
\alias{tanpi}
\description{
  These functions give the obvious trigonometric functions.  They
  respectively compute the cosine, sine, tangent, arc-cosine, arc-sine,
  arc-tangent, and the two-argument arc-tangent.

  \code{cospi(x)}, \code{sinpi(x)}, and \code{tanpi(x)}, compute
  \code{cos(pi*x)}, \code{sin(pi*x)}, and \code{tan(pi*x)}.
}
\usage{
cos(x)
sin(x)
tan(x)

acos(x)
asin(x)
atan(x)
atan2(y, x)

cospi(x)
sinpi(x)
tanpi(x)
}
\arguments{
   \item{x, y}{numeric or complex vectors.}
}
\details{
  The arc-tangent of two arguments \code{atan2(y, x)} returns the angle
  between the x-axis and the vector from the origin to \eqn{(x, y)},
  i.e., for positive arguments \code{atan2(y, x) == atan(y/x)}.

  Angles are in radians, not degrees, for the standard versions (i.e., a
  right angle is \eqn{\pi/2}), and in \sQuote{half-rotations} for
  \code{cospi} etc.

  \code{cospi(x)}, \code{sinpi(x)}, and \code{tanpi(x)} are accurate
  for \code{x} values which are multiples of a half.

  All except \code{atan2} are \link{internal generic} \link{primitive}
  functions: methods can be defined for them individually or via the
  \code{\link[=S3groupGeneric]{Math}} group generic.
  
  These are all wrappers to system calls of the same name (with prefix
  \code{c} for complex arguments) where available.  (\code{cospi},
  \code{sinpi}, and \code{tanpi} are part of a C11 extension
  and provided by e.g.\sspace{}macOS and Solaris: where not yet
  available call to \code{cos} \emph{etc} are used, with special cases
  for  multiples of a half.)
}

\value{
  \code{tanpi(0.5)} is \code{\link{NaN}}.  Similarly for other inputs
  with fractional part \code{0.5}.
}

\section{Complex values}{
   For the inverse trigonometric functions, branch cuts are defined as in
   Abramowitz and Stegun, figure 4.4, page 79.

   For \code{asin} and \code{acos}, there are two cuts, both along
   the real axis: \eqn{\left(-\infty, -1\right]}{(-Inf, -1]} and
   \eqn{\left[1, \infty\right)}{[1, Inf)}.

   For \code{atan} there are two cuts, both along the pure imaginary
   axis: \eqn{\left(-\infty i, -1i\right]}{(-1i*Inf, -1i]} and
   \eqn{\left[1i, \infty i\right)}{[1i, 1i*Inf)}.

   The behaviour actually on the cuts follows the C99 standard which
   requires continuity coming round the endpoint in a counter-clockwise
   direction.

   Complex arguments for  \code{cospi}, \code{sinpi}, and \code{tanpi}
   are not yet implemented, and they are a \sQuote{future direction} of
   ISO/IEC TS 18661-4.% but patches are welcome
}

\section{S4 methods}{
  All except \code{atan2} are S4 generic functions: methods can be defined
  for them individually or via the
  \code{\link[=S4groupGeneric]{Math}} group generic.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Abramowitz, M. and Stegun, I. A. (1972). \emph{Handbook of
    Mathematical Functions}. New York: Dover.\cr
  Chapter 4. Elementary Transcendental Functions: Logarithmic,
  Exponential, Circular and Hyperbolic Functions

  For \code{cospi}, \code{sinpi}, and \code{tanpi} the C11 extension
  ISO/IEC TS 18661-4:2015 (draft at
  \url{http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1950.pdf}).
  
}
\examples{
x <- seq(-3, 7, by = 1/8)
tx <- cbind(x, cos(pi*x), cospi(x), sin(pi*x), sinpi(x),
               tan(pi*x), tanpi(x), deparse.level=2)
op <- options(digits = 4, width = 90) # for nice formatting
head(tx)
tx[ (x \%\% 1) \%in\% c(0, 0.5) ,]
options(op)
}
\keyword{math}
