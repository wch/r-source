% $Id: SSasympOff.Rd,v 1.1 1999/11/12 13:34:35 bates Exp $
\name{SSasympOff}
\title{Asymptotic Regression Model with an Offset}
\usage{
SSasympOff(input, Asym, lrc, c0)
}
\alias{SSasympOff}
\arguments{
 \item{input}{a numeric vector of values at which to evaluate the model.}
 \item{Asym}{a numeric parameter representing the horizontal asymptote on
   the right side (very large values of \code{input}).}
 \item{lrc}{a numeric parameter representing the natural logarithm of
   the rate constant.}
 \item{c0}{a numeric parameter representing the \code{input} for which the
   response is zero.}
}
\description{
  This \code{selfStart} model evaluates an alternative parameterization
  of the asymptotic
  regression function and the gradient with respect to those parameters.
  It has an \code{initial}
  attribute that creates initial estimates of the parameters
  \code{Asym}, \code{lrc}, and \code{c0}.
}
\value{
  a numeric vector of the same length as \code{input}.  It is the value of
  the expression \code{Asym*(1 - exp(-exp(lrc)*(input - c0)))}.  If all of
  the arguments \code{Asym}, \code{lrc}, and \code{c0} are
  names of objects, the gradient matrix with respect to these names is
  attached as an attribute named \code{gradient}.
}
\author{Jose Pinheiro and Douglas Bates}
\seealso{\code{\link{nls}}, \code{\link{selfStart}}
}
\examples{
library( nls )
data( CO2 )
CO2.Qn1 <- CO2[CO2$Plant == "Qn1", ]
SSasympOff( CO2.Qn1$conc, 32, -4, 43 )  # response only
Asym <- 32; lrc <- -4; c0 <- 43
SSasympOff( CO2.Qn1$conc, Asym, lrc, c0 ) # response and gradient
getInitial(uptake ~ SSasymp( conc, Asym, lrc, c0), data = CO2.Qn1)
## Initial values are in fact the converged values
fm1 <- nls(uptake ~ SSasymp( conc, Asym, lrc, c0), data = CO2.Qn1)
summary(fm1)
}
\keyword{models}
