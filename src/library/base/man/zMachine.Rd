% File src/library/base/man/zMachine.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2022 R Core Team
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
    Always \eqn{2^{31} - 1 = 2147483647}{2^31 - 1 = 2147483647}.}
  \item{sizeof.long}{the number of bytes in a C \samp{long} type:
    \code{4} or \code{8} (most 64-bit systems, but not Windows).}
  \item{sizeof.longlong}{the number of bytes in a C \samp{long long}
    type.  Will be zero if there is no such type, otherwise usually
    \code{8}.}
  \item{sizeof.longdouble}{the number of bytes in a C \samp{long double}
    type.  Will be zero if there is no such type (or its use was
    disabled when \R was built), otherwise possibly
    \code{12} (most 32-bit builds),  \code{16} (most 64-bit builds) or 8
    (CPUs such as ARM where for most compilers \samp{long double} is
    identical to \code{double}).}
    % 64-bit ARM Linux compilers have a software-implemented
    % quad-precison as long double
  \item{sizeof.pointer}{the number of bytes in the C \code{SEXP}
    type.  Will be \code{4} on 32-bit builds and \code{8} on 64-bit
    builds of \R.}
  \item{sizeof.time_t}{the number of \emph{bytes} in the C \code{time_t}
    type: a  64-bit \code{time_t} (value \code{8}) is much preferred these
    days.  Note that this is the type used by code in \R itself, not
    necessarily the \emph{system} type if \R was configured with
    \option{--with-internal-tzcode} as also used on Windows.}

  \item{longdouble.eps, longdouble.neg.eps, longdouble.digits, ...}{
    introduced in \R 4.0.0. When
    \code{\link{capabilities}("long.double")} is true, there are 10 such
    \code{"longdouble.\var{kind}"} values, specifying the \samp{long double}
    property corresponding to its \code{"double.*"} counterpart.  See
    also \sQuote{Note}.}
}
\details{
  The algorithm is based on Cody's (1988) subroutine \code{MACHAR}.  As all
  current implementations of \R use 32-bit integers and use \abbr{IEC} 60559
  floating-point (double precision) arithmetic, the \code{"integer"} and
  \code{"double"} related values are the same for almost all \R builds.
  % differing sizeof.(long|longdouble|pointer)

  Note that on most platforms smaller positive values than
  \code{.Machine$double.xmin} can occur.  On a typical \R platform the
  smallest positive double is about \code{5e-324}.
}
\note{
  In the (typical) case where \code{\link{capabilities}("long.double")} is
  true, \R uses the \samp{long double} C type in quite a few places internally
  for accumulators in e.g.\sspace{}\code{\link{sum}}, reading non-integer
  numeric constants into (binary) double precision numbers, or arithmetic
  such as \code{x \%\% y}; also, \samp{long double} can be read by
  \code{\link{readBin}}.
  \cr%--------------------------------
  For this reason, in that case, \code{.Machine} contains ten further components,
  \code{longdouble.eps}, \code{*.neg.eps}, \code{*.digits}, \code{*.rounding}
  \code{*.guard}, \code{*.ulp.digits}, \code{*.neg.ulp.digits},
  \code{*.exponent}, \code{*.min.exp}, and \code{*.max.exp}, computed
  entirely analogously to their \code{double.*} counterparts, see there.

  \code{sizeof.longdouble} only tells you the amount of storage
  allocated for a long double.  Often what is stored is the 80-bit extended
  double type of \abbr{IEC} 60559, padded to the double alignment used on the
  platform --- this seems to be the case for the common \R platforms
  using ix86 and x86_64 chips.  There are other implementation of long
  double, usually in software for example on \I{Sparc} Solaris and \I{AIX}.

  Note that it is legal for a platform to have a \samp{long double} C
  type which is identical to the \samp{double} type --- this happens on
  ARM CPUs.  In that case \code{\link{capabilities}("long.double")} will
  be false but on versions of \R prior to 4.0.4, \code{.Machine} may contain
  \code{"longdouble.\var{kind}"} elements.
}
\source{
  Uses a C translation of Fortran code in the reference, modified by the
  R Core Team to defeat over-optimization in modern compilers.
}
\references{
  Cody, W. J. (1988).
  MACHAR: A subroutine to dynamically determine machine parameters.
  \emph{Transactions on Mathematical Software}, \bold{14}(4), 303--311.
  \doi{10.1145/50063.51907}.
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
