% File src/library/base/man/Log.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{log}
\alias{log}
\alias{logb}
\alias{log10}
\alias{log2}
\alias{log1p}
\alias{exp}
\alias{expm1}
\title{Logarithms and Exponentials}
\usage{
log(x, base = exp(1))
logb(x, base = exp(1))
log10(x)
log2(x)

log1p(x)

exp(x)
expm1(x)
}
\arguments{
  \item{x}{a numeric or complex vector.}
  \item{base}{a positive or complex number: the base with respect to which
    logarithms are computed.  Defaults to \eqn{e}=\code{exp(1)}.}
  }
\description{
  \code{log} computes logarithms, by default natural logarithms,
  \code{log10} computes common (i.e., base 10) logarithms, and
  \code{log2} computes binary (i.e., base 2) logarithms.
  The general form \code{log(x, base)} computes logarithms with base
  \code{base}.

  \code{log1p(x)} computes \eqn{\log(1+x)}{log(1+x)} accurately also for
  \eqn{|x| \ll 1}{|x| << 1}.

  \code{exp} computes the exponential function.

  \code{expm1(x)} computes \eqn{\exp(x) - 1}{exp(x) - 1} accurately also for
  \eqn{|x| \ll 1}{|x| << 1}.
}
\value{
  A vector of the same length as \code{x} containing the transformed
  values.  \code{log(0)} gives \code{-Inf}, and \code{log(x)} for
  negative values of \code{x} is \code{NaN}.  \code{exp(-Inf)} is \code{0}.

  For complex inputs to the log functions, the value is a complex number
  with imaginary part in the range \eqn{[-\pi, \pi]}{[-pi, pi]}: which
  end of the range is used might be platform-specific.
}
\details{
  All except \code{logb} are generic functions: methods can be defined
  for them individually or via the \code{\link[=S3groupGeneric]{Math}}
  group generic.

  \code{log10} and \code{log2} are only convenience wrappers, but logs
  to bases 10 and 2 (whether computed \emph{via} \code{log} or the wrappers)
  will be computed more efficiently and accurately where supported by the OS.
  Methods can be set for them individually (and otherwise methods for
  \code{log} will be used).

  \code{logb} is a wrapper for \code{log} for compatibility with S.  If
  (S3 or S4) methods are set for \code{log} they will be dispatched.
  Do not set S4 methods on \code{logb} itself.

  All except \code{log} are \link{primitive} functions.
}
\section{S4 methods}{
  \code{exp}, \code{expm1}, \code{log}, \code{log10}, \code{log2} and
  \code{log1p} are S4 generic and are members of the
  \code{\link[=S4groupGeneric]{Math}} group generic.

  Note that this means that the S4 generic for \code{log} has a
  signature with only one argument, \code{x}, but that \code{base} can
  be passed to methods (but will not be used for method selection).  On
  the other hand, if you only set a method for the \code{Math} group
  generic then \code{base} argument of \code{log} will be ignored for
  your class.
}
\source{
  \code{log1p} and \code{expm1} may be taken from the operating system,
  but if not available there then they are based on the Fortran subroutine
  \code{dlnrel} by W. Fullerton of Los Alamos Scientific Laboratory (see
  \url{http://www.netlib.org/slatec/fnlib/dlnrel.f} and (for small x) a
  single Newton step for the solution of \code{log1p(y) = x}
  respectively.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
  (for \code{log}, \code{log10} and \code{exp}.)

  Chambers, J. M. (1998)
  \emph{Programming with Data.  A Guide to the S Language}.
  Springer. (for \code{logb}.)
}
\seealso{
  \code{\link{Trig}},
  \code{\link{sqrt}},
  \code{\link{Arithmetic}}.
}
\examples{
log(exp(3))
log10(1e7) # = 7

x <- 10^-(1+2*1:9)
cbind(x, log(1+x), log1p(x), exp(x)-1, expm1(x))
}
\keyword{math}
