% File src/library/base/man/Hyperbolic.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

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
  inverses, arc-cosine, arc-sine, arc-tangent (or \sQuote{\emph{area cosine}},
  etc).
}
\arguments{
   \item{x}{a numeric or complex vector}
}
\details{
   These are \link{internal generic} \link{primitive} functions: methods
   can be defined for them individually or via the
   \code{\link[=S3groupGeneric]{Math}} group generic.

   Branch cuts are consistent with the inverse trigonometric functions
   \code{asin} \emph{et seq}, and agree with those defined in Abramowitz
   and Stegun, figure 4.7, page 86.   The behaviour actually on the cuts
   follows the C99 standard which requires continuity coming round the
   endpoint in a counter-clockwise direction.
}
\section{S4 methods}{
  All are S4 generic functions: methods can be defined
  for them individually or via the
  \code{\link[=S4groupGeneric]{Math}} group generic.
}
\seealso{
  The trigonometric functions, \code{\link{cos}}, \code{\link{sin}},
  \code{\link{tan}}, and their inverses
  \code{\link{acos}}, \code{\link{asin}}, \code{\link{atan}}.

  The logistic distribution function \code{\link{plogis}} is a shifted
  version of \code{tanh()} for numeric \code{x}.
}
\references{
  Abramowitz, M. and Stegun, I. A. (1972)
  \emph{Handbook of Mathematical Functions.} New York: Dover.\cr
  Chapter 4. Elementary Transcendental Functions: Logarithmic,
  Exponential, Circular and Hyperbolic Functions
}
\keyword{math}
