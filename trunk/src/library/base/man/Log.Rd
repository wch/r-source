\name{log}
\title{Logarithms and Exponentials}
\usage{
log(x, base = exp(1))
logb(x, base = exp(1))
log10(x)
log2(x)

exp(x)
expm1(x)

log1p(x)
}
\alias{log}
\alias{logb}
\alias{log1p}
\alias{log10}
\alias{log2}
\alias{exp}
\alias{expm1}
\arguments{
  \item{x}{a numeric or complex vector.}
  \item{base}{positive number.  The base with respect to which
    logarithms are computed.  Defaults to \eqn{e}=\code{exp(1)}.}
  }
\description{
  \code{log} computes natural logarithms,
  \code{log10} computes common (i.e., base 10) logarithms, and
  \code{log2} computes binary (i.e., base 2) logarithms.
  The general form \code{logb(x, base)} computes logarithms with base
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
  values.  \code{log(0)} gives \code{-Inf} (when available).
}
\note{
  \code{log} and \code{logb} are the same thing in \R, but \code{logb}
  is preferred if \code{base} is specified, for S-PLUS compatibility.
}
\details{
  \code{exp} and \code{log} are generic functions: methods can be defined
  for them individually or via the \code{\link[base:groupGeneric]{Math}}
  group generic.

  \code{log10} and \code{log2} are only special cases, but will be computed
  more efficiently and accurately where supported by the OS.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
  (for \code{log}, \code{\log10} and \code{exp}.)

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
