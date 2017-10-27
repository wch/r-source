% File src/library/stats/man/SSasympOrig.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{SSasympOrig}
\alias{SSasympOrig}
\title{Self-Starting Nls Asymptotic Regression Model through the Origin}
\encoding{UTF-8}
\usage{
SSasympOrig(input, Asym, lrc)
}
\arguments{
 \item{input}{a numeric vector of values at which to evaluate the model.}
 \item{Asym}{a numeric parameter representing the horizontal asymptote.}
 \item{lrc}{a numeric parameter representing the natural logarithm of
   the rate constant.}
}
\description{
  This \code{\link{selfStart}} model evaluates the asymptotic regression
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
\author{\enc{José}{Jose} Pinheiro and Douglas Bates}
\seealso{\code{\link{nls}}, \code{\link{selfStart}}
}
\examples{\donttest{
Lob.329 <- Loblolly[ Loblolly$Seed == "329", ]
SSasympOrig(Lob.329$age, 100, -3.2)  # response only
local({   Asym <- 100; lrc <- -3.2
  SSasympOrig(Lob.329$age, Asym, lrc) # response and gradient
})
getInitial(height ~ SSasympOrig(age, Asym, lrc), data = Lob.329)
## Initial values are in fact the converged values
fm1 <- nls(height ~ SSasympOrig(age, Asym, lrc), data = Lob.329)
summary(fm1)
}

## Visualize the SSasympOrig()  model  parametrization :

  xx <- seq(0, 5, len = 101)
  yy <- 5 * (1- exp(-xx * log(2)))
  stopifnot( all.equal(yy, SSasympOrig(xx, Asym = 5, lrc = log(log(2)))) )

  require(graphics)
  op <- par(mar = c(0, 0, 3.5, 0))
  plot(xx, yy, type = "l", axes = FALSE, ylim = c(0,5), xlim = c(-1/4, 5),
       xlab = "", ylab = "", lwd = 2,
       main = quote("Parameters in the SSasympOrig model"~~ f[phi](x)))
  mtext(quote(list(phi[1] == "Asym", phi[2] == "lrc")))
  usr <- par("usr")
  arrows(usr[1], 0, usr[2], 0, length = 0.1, angle = 25)
  arrows(0, usr[3], 0, usr[4], length = 0.1, angle = 25)
  text(usr[2] - 0.2, 0.1, "x", adj = c(1, 0))
  text(   -0.1,   usr[4], "y", adj = c(1, 1))
  abline(h = 5, lty = 3)
  axis(2, at = 5*c(1/2,1), labels= expression(frac(phi[1],2), phi[1]), pos=0, las=1)
  arrows(c(.3,.7), 5/2,
         c(0, 1 ), 5/2, length = 0.08, angle = 25)
  text(   0.5,     5/2, quote(t[0.5]))
  text(   1 +.4,   5/2,
       quote({f(t[0.5]) == frac(phi[1],2)}~{} \%=>\% {}~~{t[0.5] == frac(log(2), e^{phi[2]})}),
       adj = c(0, 0.5))
  par(op)
}
\keyword{models}
