\name{Log}
\title{Logarithms and Exponentials}
\usage{
log(x)
log10(x)
log2(x)
exp(x)
}
\alias{log}
\alias{log10}
\alias{log2}
\alias{exp}
\description{
\code{log} computes natural logarithms,
\code{log10} computes common logarithms
\code{log2} computes binary (i.e. base 2) logarithms
and
\code{exp} computes the exponential function.

Each of these functions takes a single numeric or complex
vector as argument and returns a vector of the same
length containing the transformed values.}
\seealso{
\code{\link{Trig}}, \code{\link{Math}}, \code{\link{Arithmetic}}.
}
\examples{
log10(10^7) == 10^log10(7)
log2(2^pi) == 2^log2(pi)
Mod(pi - log(exp(pi*1i)) / 1i) < .Machine$double.eps
Mod(1+exp(pi*1i)) < .Machine$double.eps
}
