% File src/library/stats/man/SSasymp.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2010 R Core Team
% Distributed under GPL 2 or later

\name{SSasymp}
\encoding{UTF-8}
\title{Self-Starting Nls Asymptotic Regression Model}
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
\author{\enc{José}{Jose} Pinheiro and Douglas Bates}
\seealso{
  \code{\link{nls}}, \code{\link{selfStart}}
}
\examples{
\testonly{options(show.nls.convergence=FALSE)}
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
\dontshow{
require(graphics)

  xx <- seq(0, 5, len = 101)
  yy <- 5 - 4 * exp(-xx/(2*log(2)))
  par(mar = c(0, 0, 4.1, 0))
  plot(xx, yy, type = "l", axes = FALSE, ylim = c(0,6), xlim = c(-1, 5),
       xlab = "", ylab = "", lwd = 2,
       main = "Parameters in the SSasymp model")
  usr <- par("usr")
  arrows(usr[1], 0, usr[2], 0, length = 0.1, angle = 25)
  arrows(0, usr[3], 0, usr[4], length = 0.1, angle = 25)
  text(usr[2] - 0.2, 0.1, "x", adj = c(1, 0))
  text(-0.1, usr[4], "y", adj = c(1, 1))
  abline(h = 5, lty = 2, lwd = 0)
  arrows(-0.8, 2.1, -0.8, 0, length = 0.1, angle = 25)
  arrows(-0.8, 2.9, -0.8, 5, length = 0.1, angle = 25)
  text(-0.8, 2.5, expression(phi[1]), adj = c(0.5, 0.5))
  segments(-0.4, 1, 0, 1, lty = 2, lwd = 0.75)
  arrows(-0.3, 0.25, -0.3, 0, length = 0.07, angle = 25)
  arrows(-0.3, 0.75, -0.3, 1, length = 0.07, angle = 25)
  text(-0.3, 0.5, expression(phi[2]), adj = c(0.5, 0.5))
  segments(1, 3.025, 1, 4, lty = 2, lwd = 0.75)
  arrows(0.2, 3.5, 0, 3.5, length = 0.08, angle = 25)
  arrows(0.8, 3.5, 1, 3.5, length = 0.08, angle = 25)
  text(0.5, 3.5, expression(t[0.5]), adj = c(0.5, 0.5))
}
}
\keyword{models}
