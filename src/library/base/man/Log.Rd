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
  \eqn{|x| \ll 1}{|x| << 1} (and less accurately when \eqn{x \approx
    -1}{x is approximately -1}).

  \code{exp} computes the exponential function.

  \code{expm1(x)} computes \eqn{\exp(x) - 1}{exp(x) - 1} accurately also for
  \eqn{|x| \ll 1}{|x| << 1}.
}
\value{
  A vector of the same length as \code{x} containing the transformed
  values.  \code{log(0)} gives \code{-Inf}.
}
\note{
  \code{log} and \code{logb} are the same thing in \R, but \code{logb}
  is preferred if \code{base} is specified, for S-PLUS compatibility.
}
\details{
  All except \code{logb} are generic functions: methods can be defined
  for them individually or via the \code{\link[base:groupGeneric]{Math}}
  group generic.

  \code{log10} and \code{log2} are only convenience wrappers, but logs
  to bases 10 and 2 (whether computed \emph{via} \code{log} or the wrappers)
  will be computed more efficiently and accurately where supported by the OS.
  Methods can be set for them individually (and otherwise methods for
  \code{log} will be used).

  \code{logb} is a wrapper for \code{log} for compatibility with S.  If
  (S3 or S4) methods are set for \code{log} they will be dispatched.
  Do not set S4 methods on \code{logb} itself.
}
\section{S4 methods}{
  \code{exp}, \code{expm1}, \code{log}, \code{log10}, \code{log2} and
  \code{log1p} are S4 generic and are members of the
  \code{\link[methods:S4groupGeneric]{Math}} group generic.  Note that
  this means that the S4 generic for \code{log} has a signature with
  only one argument, \code{x}, but that \code{base} can be passed to
  methods but will not be used for method selection.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
  (for \code{log}, \code{log10} and \code{exp}.)

  Chambers, J. M. (1998)
  \emph{Programming with Data. A Guide to the S Language}.
  Springer. (for \code{logb}.)
}
\seealso{
  \code{\link{Trig}},
  \code{\link{sqrt}},
  \code{\link{Arithmetic}}.
}
\examples{
log(exp(3))
log10(1e7)# = 7

x <- 10^-(1+2*1:9)
cbind(x, log(1+x), log1p(x), exp(x)-1, expm1(x))
}
\keyword{math}
