\name{Log}
\title{Logarithms and Exponentials}
\usage{
log(x)
log(x, base)
log10(x)
log2(x)
exp(x)
}
\alias{log}
\alias{log10}
\alias{log2}
\alias{exp}
\arguments{
  \item{x}{a numeric of complex vector}
  }
\description{
  \code{log} computes natural logarithms,
  \code{log10} computes common (i.e., base 10) logarithms, and
  \code{log2} computes binary (i.e., base 2) logarithms.
  The general form \code{log(x, base)} computes logarithms with base
  \code{base} (\code{log10} and \code{log2} are only special cases).

  \code{exp} computes the exponential function.
}
\value{
  A vector of the same length as \code{x} containing the transformed
  values.
}
\seealso{
  \code{\link{Trig}}, \code{\link{Math}}, \code{\link{Arithmetic}}.
}
\examples{
log(exp(3))
log10(30) == log(30, 10)
log10(10^7) == 10^log10(7)        
log2(2^pi) == 2^log2(pi)
Mod(pi - log(exp(pi*1i)) / 1i) < .Machine$double.eps
Mod(1+exp(pi*1i)) < .Machine$double.eps
}
