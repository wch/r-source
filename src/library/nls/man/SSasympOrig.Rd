% $Id: SSasympOrig.Rd,v 1.1 1999/11/12 13:34:35 bates Exp $
\name{SSasympOrig}
\title{Asymptotic Regression Model through the Origin}
\usage{
SSasympOrig(input, Asym, lrc)
}
\alias{SSasympOrig}
\arguments{
 \item{input}{a numeric vector of values at which to evaluate the model.}
 \item{Asym}{a numeric parameter representing the horizontal asymptote.}
 \item{lrc}{a numeric parameter representing the natural logarithm of
   the rate constant.}
}
\description{
  This \code{selfStart} model evaluates the asymptotic regression
  function through the origin and its gradient.  It has an
  \code{initial} attribute that will evaluate initial estimates of the
  parameters \code{Asym} and \code{lrc} for a given set of data.
}
\value{
  a numeric vector of the same length as \code{input}.  It is the value of
  the expression \code{Asym*(1 - exp(-exp(lrc)*input))}.  If all of
  the arguments \code{Asym} and \code{lrc} are
  names of objects, the gradient matrix with respect to these names is
  attached as an attribute named \code{gradient}.
}
\author{Jose Pinheiro and Douglas Bates}
\seealso{\code{\link{nls}}, \code{\link{selfStart}}
}
\examples{
library( nls )
data( Loblolly )
Lob.329 <- Loblolly[ Loblolly$Seed == "329", ]
SSasympOrig( Lob.329$age, 100, -3.2 )  # response only
Asym <- 100; lrc <- -3.2
SSasympOrig( Lob.329$age, Asym, lrc ) # response and gradient
getInitial(height ~ SSasympOrig(age, Asym, lrc), data = Lob.329)
## Initial values are in fact the converged values
fm1 <- nls(height ~ SSasympOrig( age, Asym, lrc), data = Lob.329)
summary(fm1)
}
\keyword{models}
