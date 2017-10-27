% File src/library/stats/man/SSasymp.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
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

  Note that \code{\link{SSweibull}()} generalizes this asymptotic model
  with an extra parameter.
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
\dontshow{options(show.nls.convergence=FALSE)}
Lob.329 <- Loblolly[ Loblolly$Seed == "329", ]
SSasymp( Lob.329$age, 100, -8.5, -3.2 )   # response only
local({
  Asym <- 100 ; resp0 <- -8.5 ; lrc <- -3.2
  SSasymp( Lob.329$age, Asym, resp0, lrc) # response _and_ gradient
})
getInitial(height ~ SSasymp( age, Asym, resp0, lrc), data = Lob.329)
## Initial values are in fact the converged values
fm1 <- nls(height ~ SSasymp( age, Asym, resp0, lrc), data = Lob.329)
summary(fm1)

## Visualize the SSasymp()  model  parametrization :

  xx <- seq(-.3, 5, len = 101)
  ##  Asym + (R0-Asym) * exp(-exp(lrc)* x) :
  yy <- 5 - 4 * exp(-xx / exp(3/4))
  stopifnot( all.equal(yy, SSasymp(xx, Asym = 5, R0 = 1, lrc = -3/4)) )
  require(graphics)
  op <- par(mar = c(0, .2, 4.1, 0))
  plot(xx, yy, type = "l", axes = FALSE, ylim = c(0,5.2), xlim = c(-.3, 5),
       xlab = "", ylab = "", lwd = 2,
       main = quote("Parameters in the SSasymp model " ~
                    {f[phi](x) == phi[1] + (phi[2]-phi[1])*~e^{-e^{phi[3]}*~x}}))
  mtext(quote(list(phi[1] == "Asym", phi[2] == "R0", phi[3] == "lrc")))
  usr <- par("usr")
  arrows(usr[1], 0, usr[2], 0, length = 0.1, angle = 25)
  arrows(0, usr[3], 0, usr[4], length = 0.1, angle = 25)
  text(usr[2] - 0.2, 0.1, "x", adj = c(1, 0))
  text(     -0.1, usr[4], "y", adj = c(1, 1))
  abline(h = 5, lty = 3)
  arrows(c(0.35, 0.65), 1,
         c(0  ,  1   ), 1, length = 0.08, angle = 25); text(0.5, 1, quote(1))
  y0 <- 1 + 4*exp(-3/4) ; t.5 <- log(2) / exp(-3/4) ; AR2 <- 3 # (Asym + R0)/2
  segments(c(1, 1), c( 1, y0),
           c(1, 0), c(y0,  1),  lty = 2, lwd = 0.75)
  text(1.1, 1/2+y0/2, quote((phi[1]-phi[2])*e^phi[3]), adj = c(0,.5))
  axis(2, at = c(1,AR2,5), labels= expression(phi[2], frac(phi[1]+phi[2],2), phi[1]),
       pos=0, las=1)
  arrows(c(.6,t.5-.6), AR2,
         c(0, t.5   ), AR2, length = 0.08, angle = 25)
  text(   t.5/2,   AR2, quote(t[0.5]))
  text(   t.5 +.4, AR2,
       quote({f(t[0.5]) == frac(phi[1]+phi[2],2)}~{} \%=>\% {}~~
                {t[0.5] == frac(log(2), e^{phi[3]})}), adj = c(0, 0.5))
  par(op)
}
\keyword{models}
