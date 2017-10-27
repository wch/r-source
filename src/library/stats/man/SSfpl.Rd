% File src/library/stats/man/SSfpl.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{SSfpl}
\title{Self-Starting Nls Four-Parameter Logistic Model}
\alias{SSfpl}
\encoding{UTF-8}
\usage{
SSfpl(input, A, B, xmid, scal)
}
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
  function and its gradient.  It has an \code{initial} attribute computing
  initial estimates of the parameters \code{A}, \code{B},
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
local({
  A <- 13; B <- 368; xmid <- 14; scal <- 6
  SSfpl(Chick.1$Time, A, B, xmid, scal) # response _and_ gradient
})
print(getInitial(weight ~ SSfpl(Time, A, B, xmid, scal), data = Chick.1),
      digits = 5)
## Initial values are in fact the converged values
fm1 <- nls(weight ~ SSfpl(Time, A, B, xmid, scal), data = Chick.1)
summary(fm1)

## Visualizing the  SSfpl()  parametrization
  xx <- seq(-0.5, 5, len = 101)
  yy <- 1 + 4 / (1 + exp((2-xx))) # == SSfpl(xx, *) :
  stopifnot( all.equal(yy, SSfpl(xx, A = 1, B = 5, xmid = 2, scal = 1)) )
  require(graphics)
  op <- par(mar = c(0, 0, 3.5, 0))
  plot(xx, yy, type = "l", axes = FALSE, ylim = c(0,6), xlim = c(-1, 5),
       xlab = "", ylab = "", lwd = 2,
       main = "Parameters in the SSfpl model")
  mtext(quote(list(phi[1] == "A", phi[2] == "B", phi[3] == "xmid", phi[4] == "scal")))
  usr <- par("usr")
  arrows(usr[1], 0, usr[2], 0, length = 0.1, angle = 25)
  arrows(0, usr[3], 0, usr[4], length = 0.1, angle = 25)
  text(usr[2] - 0.2, 0.1, "x", adj = c(1, 0))
  text(     -0.1, usr[4], "y", adj = c(1, 1))
  abline(h = c(1, 5), lty = 3)
  arrows(-0.8, c(2.1, 2.9),
         -0.8, c(0,   5  ), length = 0.1, angle = 25)
  text  (-0.8, 2.5, quote(phi[1]))
  arrows(-0.3, c(1/4, 3/4),
         -0.3, c(0,   1  ), length = 0.07, angle = 25)
  text  (-0.3, 0.5, quote(phi[2]))
  text(2, -.1, quote(phi[3]))
  segments(c(2,3,3), c(0,3,4), # SSfpl(x = xmid = 2) = 3
           c(2,3,2), c(3,4,3),    lty = 2, lwd = 0.75)
  arrows(c(2.3, 2.7), 3,
         c(2.0, 3  ), 3, length = 0.08, angle = 25)
  text(      2.5,     3, quote(phi[4])); text(3.1, 3.5, "1")
  par(op)
}
\keyword{models}
