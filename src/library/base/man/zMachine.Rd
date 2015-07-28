% File src/library/base/man/zMachine.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{.Machine}
\alias{.Machine}
\concept{long double}
\title{Numerical Characteristics of the Machine}
\usage{
.Machine
}
\description{
  \code{.Machine} is a variable holding information on the numerical
  characteristics of the machine \R is running on, such as the largest
  double or integer and the machine's precision.
}
\value{
  A list with components
  \item{double.eps}{the smallest positive floating-point number
    \code{x} such that \code{1 + x != 1}.  It equals
    \code{double.base ^ ulp.digits} if either \code{double.base} is 2 or
    \code{double.rounding} is 0;  otherwise, it is
    \code{(double.base ^ double.ulp.digits) / 2}.  Normally
    \code{2.220446e-16}.}
  \item{double.neg.eps}{a small positive floating-point number \code{x}
    such that \code{1 - x != 1}.  It equals
    \code{double.base ^ double.neg.ulp.digits} if \code{double.base} is 2
    or \code{double.rounding} is 0;  otherwise, it is
    \code{(double.base ^ double.neg.ulp.digits) / 2}.  Normally
    \code{1.110223e-16}. As \code{double.neg.ulp.digits} is bounded
    below by \code{-(double.digits + 3)}, \code{double.neg.eps} may not
    be the smallest number that can alter 1 by subtraction.}
  \item{double.xmin}{the smallest non-zero normalized
    floating-point number, a power of the radix, i.e.,
    \code{double.base ^ double.min.exp}. Normally \code{2.225074e-308}.}
  \item{double.xmax}{the largest normalized floating-point number.
    Typically, it is equal to \code{(1 - double.neg.eps) *
      double.base ^ double.max.exp}, but
    on some machines it is only the second or third largest such
    number, being too small by 1 or 2 units in the last digit of the
    significand.  Normally \code{1.797693e+308}.  Note that larger
    unnormalized numbers can occur.}
  \item{double.base}{the radix for the floating-point representation:
    normally \code{2}.}
  \item{double.digits}{the number of base digits in the floating-point
    significand: normally \code{53}.}
  \item{double.rounding}{the rounding action, one of\cr
    0 if floating-point addition chops; \cr
    1 if floating-point addition rounds, but not in the IEEE style; \cr
    2 if floating-point addition rounds in the IEEE style; \cr
    3 if floating-point addition chops, and there is partial underflow; \cr
    4 if floating-point addition rounds, but not in the IEEE style, and
    there is partial underflow; \cr
    5 if floating-point addition rounds in the IEEE style, and there is
    partial underflow.\cr
    Normally \code{5}.}
  \item{double.guard}{the number of guard digits for multiplication
    with truncating arithmetic.  It is 1 if floating-point arithmetic
    truncates and more than \code{double digits} base-\code{double.base} digits
    participate in the post-normalization shift of the floating-point
    significand in multiplication, and 0 otherwise.\cr
    Normally \code{0}.}
  \item{double.ulp.digits}{the largest negative integer \code{i} such
    that \code{1 + double.base ^ i != 1}, except that it is bounded below by
    \code{-(double.digits + 3)}.  Normally \code{-52}.}
  \item{double.neg.ulp.digits}{the largest negative integer \code{i}
    such that \code{1 - double.base ^ i != 1}, except that it is bounded
    below by \code{-(double.digits + 3)}. Normally \code{-53}.}
  \item{double.exponent}{
    the number of bits (decimal places if \code{double.base} is 10) reserved
    for the representation of the exponent (including the bias or sign)
    of a floating-point number.  Normally \code{11}.}
  \item{double.min.exp}{
    the largest in magnitude negative integer \code{i} such that
    \code{double.base ^ i} is positive and normalized.  Normally \code{-1022}.}
  \item{double.max.exp}{
    the smallest positive power of \code{double.base} that overflows.  Normally
    \code{1024}.}
  \item{integer.max}{the largest integer which can be represented.
    Always \eqn{2^31 - 1 = 2147483647}.}
  \item{sizeof.long}{the number of bytes in a C \code{long} type:
    \code{4} or \code{8} (most 64-bit systems, but not Windows).}
  \item{sizeof.longlong}{the number of bytes in a C \code{long long}
    type.  Will be zero if there is no such type, otherwise usually
    \code{8}.}
  \item{sizeof.longdouble}{the number of bytes in a C \code{long double}
    type.  Will be zero if there is no such type (or its use was
    disabled when \R was built), otherwise possibly
    \code{12} (most 32-bit builds) or \code{16} (most 64-bit builds).}
  \item{sizeof.pointer}{the number of bytes in a C \code{SEXP}
    type.  Will be \code{4} on 32-bit builds and \code{8} on 64-bit
    builds of \R.}
}
\details{
  The algorithm is based on Cody's (1988) subroutine MACHAR.  As all
  current implementations of \R use 32-bit integers and use IEC 60559
  floating-point (double precision) arithmetic, all but three of the
  last four values are the same for almost all \R builds. % sizeof.(long|longdouble|pointer)

  Note that on most platforms smaller positive values than
  \code{.Machine$double.xmin} can occur.  On a typical \R platform the
  smallest positive double is about \code{5e-324}.
}
\note{
  \code{sizeof.longdouble} only tells you the amount of storage
  allocated for a long double (which are normally used internally by \R for
  accumulators in e.g.\sspace{}\code{\link{sum}}, and can be read by
  \code{\link{readBin}}).  Often what is stored is the 80-bit extended
  double type of IEC 60559, padded to the double alignment used on the
  platform --- this seems to be the case for the common \R platforms
  using ix86 and x86_64 chips.
}
\source{
  Uses a C translation of Fortran code in the reference, modified by the
  R Core Team to defeat over-optimization in recent compilers.
}
\references{
  Cody, W. J. (1988)
  MACHAR: A subroutine to dynamically determine machine parameters.
  \emph{Transactions on Mathematical Software}, \bold{14}, 4, 303--311.
}
\seealso{
  \code{\link{.Platform}} for details of the platform.
}
\examples{
.Machine
## or for a neat printout
noquote(unlist(format(.Machine)))
}
\keyword{sysdata}
\keyword{programming}
\keyword{math}
