\name{log}
\title{Logarithms and Exponentials}
\usage{
log(x)
log(x, base)
log10(x)
log2(x)
exp(x)
log1p(x)
}
\alias{log}
\alias{log1p}
\alias{log10}
\alias{log2}
\alias{exp}
\arguments{
  \item{x}{a numeric or complex vector.}
  \item{base}{positive number. The base with respect to which
    logarithms are computed.  Defaults to \eqn{e}=\code{exp(1)}.}
  }
\description{
  \code{log} computes natural logarithms,
  \code{log10} computes common (i.e., base 10) logarithms, and
  \code{log2} computes binary (i.e., base 2) logarithms.
  The general form \code{log(x, base)} computes logarithms with base
  \code{base} (\code{log10} and \code{log2} are only special cases).

  \code{log1p(x)} computes log\eqn{1+x} accurately also for
  \eqn{|x| \ll 1}{|x| << 1} (and less accurately when x ~= -1).

  \code{exp} computes the exponential function.
}
\value{
  A vector of the same length as \code{x} containing the transformed
  values.  \code{log(0)} gives \code{-Inf} (when available).
}
\seealso{\code{\link{Trig}}, \code{\link{Math}}, \code{\link{Arithmetic}}.
}
\examples{
log(exp(3))
all.equal(log(1:10), log(1:10, exp(1)))
log10(30) == log(30, 10)
log10(1e7)# = 7
log2(2^pi) == 2^log2(pi)
Mod(pi - log(exp(pi*1i)) / 1i) < .Machine$double.eps
Mod(1+exp(pi*1i)) < .Machine$double.eps

x <- 10^-(1+2*1:9)
cbind(x, log(1+x), log1p(x))
}
\keyword{math}
