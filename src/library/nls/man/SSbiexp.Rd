% $Id: SSbiexp.Rd,v 1.1 1999/11/12 13:34:35 bates Exp $
\name{SSbiexp}
\title{Biexponential model}
\usage{
SSbiexp(input, A1, lrc1, A2, lrc2)
}
\alias{SSbiexp}
\arguments{
 \item{input}{a numeric vector of values at which to evaluate the model.}
 \item{A1}{a numeric parameter representing the multiplier of the first
   exponential.}
 \item{lrc1}{a numeric parameter representing the natural logarithm of
   the rate constant of the first exponential.}
 \item{A2}{a numeric parameter representing the multiplier of the second
   exponential.}
 \item{lrc2}{a numeric parameter representing the natural logarithm of
   the rate constant of the second exponential.}
}
\description{
  This \code{selfStart} model evaluates the biexponential model function
  and its gradient.  It has an \code{initial} attribute that 
  creates initial estimates of the parameters \code{A1}, \code{lrc1},
  \code{A2}, and \code{lrc2}.
}
\value{
  a numeric vector of the same length as \code{input}.  It is the value of
  the expression
  \code{A1*exp(-exp(lrc1)*input)+A2*exp(-exp(lrc2)*input)}.
  If all of the arguments \code{A1}, \code{lrc1}, \code{A2}, and
  \code{lrc2} are names of objects, the gradient matrix with respect to
  these names is attached as an attribute named \code{gradient}.
}
\author{Jose Pinheiro and Douglas Bates}
\seealso{\code{\link{nls}}, \code{\link{selfStart}}
}
\examples{
library( nls )
data( Indometh )
Indo.1 <- Indometh[Indometh$Subject == 1, ]
SSbiexp( Indo.1$time, 3, 1, 0.6, -1.3 )  # response only
A1 <- 3; lrc1 <- 1; A2 <- 0.6; lrc2 <- -1.3
SSbiexp( Indo.1$time, A1, lrc1, A2, lrc2 ) # response and gradient
getInitial(conc ~ SSbiexp(time, A1, lrc1, A2, lrc2), data = Indo.1)
## Initial values are in fact the converged values
fm1 <- nls(conc ~ SSbiexp(time, A1, lrc1, A2, lrc2), data = Indo.1)
summary(fm1)
}
\keyword{models}
