% File src/library/base/man/double.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
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
  \item{length}{desired length.}
  \item{x}{object to be coerced or tested.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  \code{double} creates a double-precision vector of the specified
  length.  The elements of the vector are all equal to \code{0}.
  It is identical to \code{\link{numeric}} (and \code{real}).

  \code{as.double} is a generic function.  It is identical to
  \code{as.numeric} (and \code{as.real}).  Methods should return an
  object of base type \code{"double"}.

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

  \code{as.double} attempts to coerce its argument to be of double
  type: like \code{\link{as.vector}} it strips attributes including
  names.  (To ensure that an object is of double type without
  stripping attributes, use \code{\link{storage.mode}}.)
  Character strings containing either a decimal representation or a
  hexadecimal representation (starting with \code{0x} or \code{0X}) can
  be converted. \code{as.double} for factors yields the codes underlying
  the factor levels, not the numeric representation of the labels, see
  also \code{\link{factor}}.

  \code{is.double} returns \code{TRUE} or \code{FALSE} depending on
  whether its argument is of double \link{type} or not.
}
%% keep next the same in double.Rd & numeric.Rd
\section{Note on names}{
  It is a historical anomaly that \R has three names for its
  floating-point vectors, \code{\link{double}}, \code{\link{numeric}}
  and \code{\link{real}}.

  \code{double} is the name of the \link{type}.
  \code{numeric} is the name of the \link{mode} and also of the implicit
  \link{class}.  As an S4 formal class, use \code{"numeric"} (there is a
  deprecated formal class \code{"double"}).

  \code{real} is deprecated and should not be used in new code.

  The potential confusion is that \R has used \emph{\link{mode}}
  \code{"numeric"} to mean \sQuote{double or integer}, which conflicts
  with the S4 usage.  Thus \code{is.numeric} tests the mode, not the
  class, but \code{as.numeric} (which is identical to \code{as.double})
  coerces to the class.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{integer}}, \code{\link{numeric}}, \code{\link{storage.mode}}.
}
\examples{
is.double(1)
all(double(3) == 0)
}
\keyword{classes}
