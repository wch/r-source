% File src/library/base/man/double.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{double}
\title{Double-Precision Vectors}
\alias{double}
\alias{as.double}
\alias{is.double}
\alias{single}
\alias{as.single}
\alias{as.single.default}
\description{
  Create, coerce to or test for a double-precision vector.
}
\usage{
double(length = 0)
as.double(x, \dots)
is.double(x)

single(length = 0)
as.single(x, \dots)
}
\arguments{
  \item{length}{A non-negative integer specifying the desired length.
    Double values will be coerced to integer:
    supplying an argument of length other than one is an error.}
  \item{x}{object to be coerced or tested.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  \code{double} creates a double-precision vector of the specified
  length.  The elements of the vector are all equal to \code{0}.
  It is identical to \code{\link{numeric}}.

  \code{as.double} is a generic function.  It is identical to
  \code{as.numeric}.  Methods should return an object of base type
  \code{"double"}.

  \code{is.double} is a test of double \link{type}.

  \emph{\R has no single precision data type.  All real numbers are
    stored in double precision format}.  The functions \code{as.single}
  and \code{single} are identical to \code{as.double} and \code{double}
  except they set the attribute \code{Csingle} that is used in the
  \code{\link{.C}} and \code{\link{.Fortran}} interface, and they are
  intended only to be used in that context.
}
\value{
  \code{double} creates a double-precision vector of the specified
  length.  The elements of the vector are all equal to \code{0}.

  \code{as.double} attempts to coerce its argument to be of double type:
  like \code{\link{as.vector}} it strips attributes including names.
  (To ensure that an object is of double type without stripping
  attributes, use \code{\link{storage.mode}}.)  Character strings
  containing optional whitespace followed by either a decimal
  representation or a hexadecimal representation (starting with
  \code{0x} or \code{0X}) can be converted, as can special values such
  as \code{"NA"}, \code{"NaN"}, \code{"Inf"} and \code{"infinity"},
  irrespective of case.

  \code{as.double} for factors yields the codes underlying the factor
  levels, not the numeric representation of the labels, see also
  \code{\link{factor}}.

  \code{is.double} returns \code{TRUE} or \code{FALSE} depending on
  whether its argument is of double \link{type} or not.
}
\section{Double-precision values}{
  All \R platforms are required to work with values conforming to the
  IEC 60559 (also known as IEEE 754) standard.  This basically works
  with a precision of 53 bits, and represents to that precision a range
  of absolute values from about \eqn{2 \times 10^{-308}}{2e-308} to
  \eqn{2 \times 10^{308}}{2e+308}.  It also has special values
  \code{\link{NaN}} (many of them), plus and minus infinity and plus and
  minus zero (although \R acts as if these are the same).  There are
  also \emph{denormal(ized)} (or \emph{subnormal}) numbers with absolute
  values above or below the range given above but represented to less
  precision.

  See \code{\link{.Machine}} for precise information on these limits.
  Note that ultimately how double precision numbers are handled is down
  to the CPU/FPU and compiler.

  In IEEE 754-2008/IEC60559:2011 this is called \sQuote{binary64} format.
}
%% keep next the same in double.Rd & numeric.Rd
\section{Note on names}{
  It is a historical anomaly that \R has two names for its
  floating-point vectors, \code{\link{double}} and \code{\link{numeric}}
  (and formerly had \code{real}).

  \code{double} is the name of the \link{type}.
  \code{numeric} is the name of the \link{mode} and also of the implicit
  \link{class}.  As an S4 formal class, use \code{"numeric"}.

  The potential confusion is that \R has used \emph{\link{mode}}
  \code{"numeric"} to mean \sQuote{double or integer}, which conflicts
  with the S4 usage.  Thus \code{is.numeric} tests the mode, not the
  class, but \code{as.numeric} (which is identical to \code{as.double})
  coerces to the class.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  \url{https://en.wikipedia.org/wiki/IEEE_754-1985},
  \url{https://en.wikipedia.org/wiki/IEEE_754-2008},
  \url{https://en.wikipedia.org/wiki/Double_precision},
  \url{https://en.wikipedia.org/wiki/Denormal_number}.

  \url{http://754r.ucbtest.org/} for links to information
  on the standards.
}
\seealso{
  \code{\link{integer}}, \code{\link{numeric}}, \code{\link{storage.mode}}.
}
\examples{
is.double(1)
all(double(3) == 0)
}
\keyword{classes}
