\name{.Machine}
\title{Numerical Characteristics of the Machine}
\usage{
.Machine
}
\alias{.Machine}
\description{
  \code{.Machine} is a variable holding information on the numerical
  characteristics of the machine \R is running on, such as the largest
  double or integer and the machine's precision.
}
\value{
  A list with components (for simplicity, the prefix \dQuote{double} is
  omitted in the explanations)
  \item{double.eps}{the smallest positive floating-point number
    \code{x} such that \code{1 + x != 1}.  It equals
    \code{base^ulp.digits} if either \code{base} is 2 or \code{rounding}
    is 0;  otherwise, it is \code{(base^ulp.digits) / 2}.}
  \item{double.neg.eps}{a small positive floating-point number \code{x}
    such that \code{1 - x != 1}.  It equals \code{base^neg.ulp.digits}
    if \code{base} is 2 or \code{round} is 0;  otherwise, it is
    \code{(base^neg.ulp.digits) / 2}.
    As \code{neg.ulp.digits} is bounded below by \code{-(digits + 3)},
    \code{neg.eps} may not be the smallest number that can alter 1 by
    subtraction.}
  \item{double.xmin}{the smallest non-vanishing normalized
    floating-point power of the radix, i.e., \code{base^min.exp}.}
  \item{double.xmax}{the largest finite floating-point number.
    Typically, it is equal to \code{(1 - neg.eps) * base^max.exp}, but
    on some machines it is only the second, or perhaps third, largest
    number, being too small by 1 or 2 units in the last digit of the
    significand.}
  \item{double.base}{the radix for the floating-point representation}
  \item{double.digits}{the number of base digits in the floating-point
    significand}
  \item{double.rounding}{the rounding action. \cr
    0 if floating-point addition chops; \cr
    1 if floating-point addition rounds, but not in the IEEE style; \cr
    2 if floating-point addition rounds in the IEEE style; \cr
    3 if floating-point addition chops, and there is partial underflow; \cr
    4 if floating-point addition rounds, but not in the IEEE style, and
    there is partial underflow; \cr
    5 if floating-point addition rounds in the IEEE style, and there is
    partial underflow}
  \item{double.guard}{the number of guard digits for multiplication
    with truncating arithmetic.  It is 1 if floating-point arithmetic
    truncates and more than \code{digits} base \code{base} digits
    participate in the post-normalization shift of the floating-point
    significand in multiplication, and 0 otherwise.}
  \item{double.ulp.digits}{the largest negative integer \code{i} such
    that \code{1 + base^i != 1}, except that it is bounded below by
    \code{-(digits + 3)}.}
  \item{double.neg.ulp.digits}{the largest negative integer \code{i}
    such that \code{1 - base^i != 1}, except that it is bounded below by
    \code{-(digits + 3)}.}
  \item{double.exponent}{
    the number of bits (decimal places if \code{base} is 10) reserved
    for the representation of the exponent (including the bias or sign)
    of a floating-point number}
  \item{double.min.exp}{
    the largest in magnitude negative integer \code{i} such that
    \code{base ^ i} is positive and normalized.}
  \item{double.max.exp}{
    the smallest positive power of \code{base} that overflows.}
  \item{integer.max}{the largest integer which can be represented.}
  \item{sizeof.long}{the number of bytes in a C \code{long} type.}
  \item{sizeof.longlong}{the number of bytes in a C \code{long long}
    type. Will be zero if there is no such type.}
  \item{sizeof.longdouble}{the number of bytes in a C \code{long double}
    type. Will be zero if there is no such type.}
  \item{sizeof.pointer}{the number of bytes in a C \code{SEXP}
    type.}
}
\details{
  The algorithm is based on Cody's (1988) subroutine MACHAR.

  Note that on most platforms smaller positive values than
  \code{.Machine$double.xmin} can occur.  On a typical \R platform the
  smallest positive double is about \code{5e-324}.
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
