% $Id: SSmicmen.Rd,v 1.1 2003/12/11 07:16:06 ripley Exp $
\name{SSmicmen}
\title{Michaelis-Menten Model}
\usage{
SSmicmen(input, Vm, K)
}
\alias{SSmicmen}
\arguments{
 \item{input}{a numeric vector of values at which to evaluate the model.}
 \item{Vm}{a numeric parameter representing the maximum value of the response.}
 \item{K}{a numeric parameter representing the \code{input} value at
   which half the maximum response is attained.  In the field of enzyme
   kinetics this is called the Michaelis parameter.}
}
\description{
  This \code{selfStart} model evaluates the Michaelis-Menten model and
  its gradient.  It has an \code{initial} attribute that
  will evaluate initial estimates of the parameters \code{Vm} and \code{K}
}
\value{
  a numeric vector of the same length as \code{input}.  It is the value of
  the expression \code{Vm*input/(K+input)}.  If both
  the arguments \code{Vm} and \code{K} are
  names of objects, the gradient matrix with respect to these names is
  attached as an attribute named \code{gradient}.
}
\author{Jose Pinheiro and Douglas Bates}
\seealso{\code{\link{nls}}, \code{\link{selfStart}}
}
\examples{
PurTrt <- Puromycin[ Puromycin$state == "treated", ]
SSmicmen( PurTrt$conc, 200, 0.05 )  # response only
Vm <- 200; K <- 0.05
SSmicmen( PurTrt$conc, Vm, K ) # response and gradient
getInitial(rate ~ SSmicmen(conc, Vm, K), data = PurTrt)
## Initial values are in fact the converged values
fm1 <- nls(rate ~ SSmicmen(conc, Vm, K), data = PurTrt)
summary( fm1 )
## Alternative call using the subset argument
fm2 <- nls(rate ~ SSmicmen(conc, Vm, K), data = Puromycin,
           subset = state == "treated")
summary(fm2)
\dontshow{
  xx <- seq(0, 5, len = 101)
  yy <- 5 * xx/(1+xx)
  par(mar = c(0, 0, 3.5, 0))
  plot(xx, yy, type = "l", axes = FALSE, ylim = c(0,6), xlim = c(-1, 5),
       xlab = "", ylab = "", lwd = 2,
       main = "Parameters in the SSmicmen model")
  usr <- par("usr")
  arrows(usr[1], 0, usr[2], 0, length = 0.1, angle = 25)
  arrows(0, usr[3], 0, usr[4], length = 0.1, angle = 25)
  text(usr[2] - 0.2, 0.1, "x", adj = c(1, 0))
  text(-0.1, usr[4], "y", adj = c(1, 1))
  abline(h = 5, lty = 2, lwd = 0)
  arrows(-0.8, 2.1, -0.8, 0, length = 0.1, angle = 25)
  arrows(-0.8, 2.9, -0.8, 5, length = 0.1, angle = 25)
  text(-0.8, 2.5, expression(phi[1]), adj = c(0.5, 0.5))
  segments(1, 0, 1, 2.7, lty = 2, lwd = 0.75)
  text(1, 2.7, expression(phi[2]), adj = c(0.5, 0))
}
}
\keyword{models}
