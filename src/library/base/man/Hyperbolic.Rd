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
  These functions give the obvious hyperbolic functions.  They
  respectively compute the hyperbolic cosine, sine, tangent, and their
  inverses, arc-cosine, arc-sine, arc-tangent (or \dQuote{\emph{area cosine}},
  etc).
}
\arguments{
  \item{x}{a numeric vector}
}
\details{
  These are generic functions: methods can be defined for them
  individually or via the \code{\link{Math}} group generic.
}
\seealso{
  The trigonometric functions, \code{\link{cos}}, \code{\link{sin}},
  \code{\link{tan}}, and their inverses
  \code{\link{acos}}, \code{\link{asin}}, \code{\link{atan}}.

  The logistic distribution function \code{\link{plogis}} is a shifted
  version of \code{tanh()}.
}
\keyword{math}
