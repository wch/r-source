% $Id: SSasymp.Rd,v 1.1 1999/11/12 13:34:35 bates Exp $
\name{SSasymp}
\title{Asymptotic Regression Model}
\usage{
SSasymp(input, Asym, R0, lrc)
}
\alias{SSasymp}
\arguments{
 \item{input}{a numeric vector of values at which to evaluate the model.}
 \item{Asym}{a numeric parameter representing the horizontal asymptote on
   the right side (very large values of \code{input}).}
 \item{R0}{a numeric parameter representing the response when
   \code{input} is zero.}
 \item{lrc}{a numeric parameter representing the natural logarithm of
   the rate constant.}
}
\description{
  This \code{selfStart} model evaluates the asymptotic regression
  function and its gradient.  It has an \code{initial} attribute that
  will evaluate initial estimates of the parameters \code{Asym}, \code{R0},
  and \code{lrc} for a given set of data.
}
\value{
  a numeric vector of the same length as \code{input}.  It is the value of
  the expression \code{Asym+(R0-Asym)*exp(-exp(lrc)*input)}.  If all of
  the arguments \code{Asym}, \code{R0}, and \code{lrc} are
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
SSasymp( Lob.329$age, 100, -8.5, -3.2 )  # response only
Asym <- 100
resp0 <- -8.5
lrc <- -3.2
SSasymp( Lob.329$age, Asym, resp0, lrc ) # response and gradient
getInitial(height ~ SSasymp( age, Asym, resp0, lrc), data = Lob.329)
## Initial values are in fact the converged values
fm1 <- nls(height ~ SSasymp( age, Asym, resp0, lrc), data = Lob.329)
summary(fm1)
}
\keyword{models}
