% File src/library/stats/man/SSweibull.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{SSweibull}
\title{Self-Starting Nls Weibull Growth Curve Model}
\usage{
SSweibull(x, Asym, Drop, lrc, pwr)
}
\alias{SSweibull}
\arguments{
 \item{x}{a numeric vector of values at which to evaluate the model.}
 \item{Asym}{a numeric parameter representing the horizontal asymptote on
   the right side (very small values of \code{x}).}
 \item{Drop}{a numeric parameter representing the change from
   \code{Asym} to the \code{y} intercept.}
 \item{lrc}{a numeric parameter representing the natural logarithm of
   the rate constant.}
 \item{pwr}{a numeric parameter representing the power to which \code{x}
   is raised.}
}
\description{
  This \code{selfStart} model evaluates the Weibull model for growth
  curve data and its gradient.  It has an \code{initial} attribute that
  will evaluate initial estimates of the parameters \code{Asym}, \code{Drop},
  \code{lrc}, and \code{pwr} for a given set of data.
}
\value{
  a numeric vector of the same length as \code{x}.  It is the value of
  the expression \code{Asym-Drop*exp(-exp(lrc)*x^pwr)}.  If all of
  the arguments \code{Asym}, \code{Drop}, \code{lrc}, and \code{pwr} are
  names of objects, the gradient matrix with respect to these names is
  attached as an attribute named \code{gradient}.
}
\details{
  This model is a generalization of the \code{\link{SSasymp}} model in
  that it reduces to \code{SSasymp} when \code{pwr} is unity.
}
\author{Douglas Bates}
\references{
  Ratkowsky, David A. (1983), \emph{Nonlinear Regression Modeling},
  Dekker. (section 4.4.5)
}
\seealso{\code{\link{nls}}, \code{\link{selfStart}}, \code{\link{SSasymp}}
}
\examples{
Chick.6 <- subset(ChickWeight, (Chick == 6) & (Time > 0))
SSweibull(Chick.6$Time, 160, 115, -5.5, 2.5)   # response only
local({ Asym <- 160; Drop <- 115; lrc <- -5.5; pwr <- 2.5
  SSweibull(Chick.6$Time, Asym, Drop, lrc, pwr) # response _and_ gradient
})
getInitial(weight ~ SSweibull(Time, Asym, Drop, lrc, pwr), data = Chick.6)
## Initial values are in fact the converged values
fm1 <- nls(weight ~ SSweibull(Time, Asym, Drop, lrc, pwr), data = Chick.6)
summary(fm1)
## Data and Fit:
plot(weight ~ Time, Chick.6, xlim = c(0, 21), main = "SSweibull() fit to Chick.6")
ux <- par("usr")[1:2]; x <- seq(ux[1], ux[2], length.out=250)
lines(x, do.call(SSweibull, c(list(x=x), coef(fm1))), col = "red", lwd=2)
As <- coef(fm1)[["Asym"]]; abline(v = 0, h = c(As, As - coef(fm1)[["Drop"]]), lty = 3)
}
\keyword{models}
