\name{Hyperbolic}
\title{Hyperbolic Functions}
\usage{
cosh(x)
sinh(x)
tanh(x)
acosh(x)
asinh(x)
atanh(x)
}
\alias{cosh}
\alias{sinh}
\alias{tanh}
\alias{acosh}
\alias{asinh}
\alias{atanh}
\description{
These functions give the obvious hyperbolic functions.
They respectively compute the hyperbolic cosine, sine, tangent,
arc-cosine, arc-sine, arc-tangent.
}
\seealso{
\code{\link{cos}}, \code{\link{sin}}, \code{\link{tan}},
\code{\link{acos}}, \code{\link{asin}}, \code{\link{atan}}.
}
\examples{
Ceps <- .Machine$double.eps # ``Computer epsilon''
x <- rnorm(500)
all(abs(cosh(x) - (exp(x) + exp(-x))/2) < 10*Ceps)
all(abs(sinh(x) - (exp(x) - exp(-x))/2) < 10*Ceps)
all(Mod(cosh(x) - cos(1i*x)) < 10*Ceps)
all(Mod(sinh(x) - sin(1i*x)/1i) < 10*Ceps)
all(abs(tanh(x)*cosh(x) - sinh(x)) < 10*Ceps)

## Inverse:
all(abs(asinh(sinh(x)) - x) < 10*Ceps)
x[abs(acosh(cosh(x)) - abs(x)) > 100*Ceps] #- imprecise for small x
all(abs(atanh(tanh(x)) - x) < 100*Ceps)

all(abs(asinh(x) - log(x + sqrt(x^2 + 1))) < 10*Ceps)
cx <- cosh(x)
all(abs(acosh(cx) - log(cx + sqrt(cx^2 - 1))) < 1000*Ceps)
}
\keyword{math}
