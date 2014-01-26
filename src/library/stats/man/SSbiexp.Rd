% File src/library/stats/man/SSbiexp.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{SSbiexp}
\encoding{UTF-8}
\title{Self-Starting Nls Biexponential model}
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
\author{\enc{José}{Jose} Pinheiro and Douglas Bates}
\seealso{\code{\link{nls}}, \code{\link{selfStart}}
}
\examples{
Indo.1 <- Indometh[Indometh$Subject == 1, ]
SSbiexp( Indo.1$time, 3, 1, 0.6, -1.3 )  # response only
A1 <- 3; lrc1 <- 1; A2 <- 0.6; lrc2 <- -1.3
SSbiexp( Indo.1$time, A1, lrc1, A2, lrc2 ) # response and gradient
print(getInitial(conc ~ SSbiexp(time, A1, lrc1, A2, lrc2), data = Indo.1),
      digits = 5)
## Initial values are in fact the converged values
fm1 <- nls(conc ~ SSbiexp(time, A1, lrc1, A2, lrc2), data = Indo.1)
summary(fm1)

## Show the model components visually
  require(graphics)

  xx <- seq(0, 5, len = 101)
  y1 <- 3.5 * exp(-4*xx)
  y2 <- 1.5 * exp(-xx)
  plot(xx, y1 + y2, type = "l", lwd=2, ylim = c(-0.2,6), xlim = c(0, 5),
       main = "Components of the SSbiexp model")
  lines(xx, y1, lty = 2, col="tomato"); abline(v=0, h=0, col="gray40")
  lines(xx, y2, lty = 3, col="blue2" )
  legend("topright", c("y1+y2", "y1 = 3.5 * exp(-4*x)", "y2 = 1.5 * exp(-x)"),
         lty=1:3, col=c("black","tomato","blue2"), bty="n")
  axis(2, pos=0, at = c(3.5, 1.5), labels = c("A1","A2"), las=2)

## and how you could have got their sum via SSbiexp():
  ySS <- SSbiexp(xx, 3.5, log(4), 1.5, log(1))
  ##                      ---          ---
  stopifnot(all.equal(y1+y2, ySS, tolerance = 1e-15))
}
\keyword{models}
