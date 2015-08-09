% File src/library/stats/man/SSfpl.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2010 R Core Team
% Distributed under GPL 2 or later

\name{SSfpl}
\encoding{UTF-8}
\title{Self-Starting Nls Four-Parameter Logistic Model}
\usage{
SSfpl(input, A, B, xmid, scal)
}
\alias{SSfpl}
\arguments{
 \item{input}{a numeric vector of values at which to evaluate the model.}
 \item{A}{a numeric parameter representing the horizontal asymptote on
   the left side (very small values of \code{input}).}
 \item{B}{a numeric parameter representing the horizontal asymptote on
   the right side (very large values of \code{input}).}
 \item{xmid}{a numeric parameter representing the \code{input} value at the
   inflection point of the curve.  The value of \code{SSfpl} will be
   midway between \code{A} and \code{B} at \code{xmid}.}
 \item{scal}{a numeric scale parameter on the \code{input} axis.}
}
\description{
  This \code{selfStart} model evaluates the four-parameter logistic
  function and its gradient.  It has an \code{initial} attribute that
  will evaluate initial estimates of the parameters \code{A}, \code{B},
  \code{xmid}, and \code{scal} for a given set of data.
}
\value{
  a numeric vector of the same length as \code{input}.  It is the value of
  the expression \code{A+(B-A)/(1+exp((xmid-input)/scal))}.  If all of
  the arguments \code{A}, \code{B}, \code{xmid}, and \code{scal} are
  names of objects, the gradient matrix with respect to these names is
  attached as an attribute named \code{gradient}.
}
\author{\enc{José}{Jose} Pinheiro and Douglas Bates}
\seealso{\code{\link{nls}}, \code{\link{selfStart}}
}
\examples{
Chick.1 <- ChickWeight[ChickWeight$Chick == 1, ]
SSfpl(Chick.1$Time, 13, 368, 14, 6)  # response only
A <- 13; B <- 368; xmid <- 14; scal <- 6
SSfpl(Chick.1$Time, A, B, xmid, scal) # response and gradient
print(getInitial(weight ~ SSfpl(Time, A, B, xmid, scal), data = Chick.1),
      digits = 5)
## Initial values are in fact the converged values
fm1 <- nls(weight ~ SSfpl(Time, A, B, xmid, scal), data = Chick.1)
summary(fm1)
\dontshow{
require(graphics)

  xx <- seq(-0.5, 5, len = 101)
  yy <- 1 + 4 / ( 1 + exp((2-xx)))
  par(mar = c(0, 0, 3.5, 0))
  plot(xx, yy, type = "l", axes = FALSE, ylim = c(0,6), xlim = c(-1, 5),
       xlab = "", ylab = "", lwd = 2,
       main = "Parameters in the SSfpl model")
  usr <- par("usr")
  arrows(usr[1], 0, usr[2], 0, length = 0.1, angle = 25)
  arrows(0, usr[3], 0, usr[4], length = 0.1, angle = 25)
  text(usr[2] - 0.2, 0.1, "x", adj = c(1, 0))
  text(-0.1, usr[4], "y", adj = c(1, 1))
  abline(h = 5, lty = 2, lwd = 0)
  arrows(-0.8, 2.1, -0.8, 0, length = 0.1, angle = 25)
  arrows(-0.8, 2.9, -0.8, 5, length = 0.1, angle = 25)
  text(-0.8, 2.5, expression(phi[1]), adj = c(0.5, 0.5))
  abline(h = 1, lty = 2, lwd = 0)
  arrows(-0.3, 0.25, -0.3, 0, length = 0.07, angle = 25)
  arrows(-0.3, 0.75, -0.3, 1, length = 0.07, angle = 25)
  text(-0.3, 0.5, expression(phi[2]), adj = c(0.5, 0.5))
  segments(2, 0, 2, 3.3, lty = 2, lwd = 0.75)
  text(2, 3.3, expression(phi[3]), adj = c(0.5, 0))
  segments(3, 1+4/(1+exp(-1)) - 0.025, 3, 2.5, lty = 2, lwd = 0.75)
  arrows(2.3, 2.7, 2.0, 2.7, length = 0.08, angle = 25)
  arrows(2.7, 2.7, 3.0, 2.7, length = 0.08, angle = 25)
  text(2.5, 2.7, expression(phi[4]), adj = c(0.5, 0.5))
}
}
\keyword{models}
